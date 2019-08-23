#!/bin/bash
#Übernimmt als Parameter den Gerätenamen, z.B. 'sdb1'.

function initialise_defaults() {
    basedir=$(dirname "$0")
    curDate=$(date +%F_%H%M)
    interactive="true"
    keyFileName=/opt/Sicherungskopien/keyfile_extern
    mountDir=$(date +%s)
    start_via_udev="false"
}

function misserfolg {
  # shellcheck disable=SC2086
  # $infobox must be splitted
  sudo -u "$curUser" $infobox "$@"
  if [[ ! -z "$mountDir" ]]
  then
    aufraeumen
    if [[ -e "/media/$mountDir" ]]
    then
      # shellcheck disable=SC2089
      del_str="\nDer Ordner \"/media/$mountDir\" muss manuell gelöscht werden."
    fi
    if [[ -e "/dev/mapper/$mountDir" ]]
    then
      del_str="$del_str\nDas Backupziel konnte nicht sauber entfernt werden. Die Entschlüsselung in \"/dev/mapper/$mountDir\" muss daher manuell gelöst werden."
    fi
    if [[ ! -z "$del_str" ]]
    then
      sudo -u "$curUser" "$infobox" "$del_str"
    fi
  fi
  exit 1
}

function aufraeumen {
  [[ -e "/media/$mountDir" ]]      && umount "/media/$mountDir"
  [[ -e "/dev/mapper/$mountDir" ]] && cryptsetup close "$mountDir"
  [[ -e "/media/$mountDir" ]]      && rmdir "/media/$mountDir"
}

function main() {
    initialise_defaults "$@"
    prepare_env_for_kde_or_gnome
    configure_display_and_user
    parse_cli_arguments "$@"

    if [[ "$interactive" == "true" ]]
    then
        # shellcheck disable=SC2086
        # $yesno_question must be splitted
        if ! sudo -u "$curUser" $yesno_question "Soll eine Sicherungskopie erstellt werden?"
        then
          exit
        fi
    fi

    decrypt_device
    mount_device
    create_backup "$@"
    aufraeumen

    if [[ "$interactive" == "true" ]]
    then
        sudo -u "$curUser" "$infobox" "Eine Sicherungskopie wurde erfolgreich angelegt."
    fi
}


function prepare_env_for_kde_or_gnome() {
    # Wir betreten die Hölle der Platformabhängigkeit.
    # Auf KDE-Systemen kann zenity nicht vorausgesetzt werden, auf
    # GNOME-Systemen hingegen kdialog nicht. Daher muss der entsprechende
    # Befehl zur Laufzeit bestimmt werden, um nicht immer zwei sehr ähnliche
    # Skripte pflegen zu müssen.
    if type kdialog > /dev/null 2> /dev/null
    then
      yesno_question="kdialog --yesno"
      pwd_prompt="kdialog --password"
      infobox="kdialog --msgbox"
    elif type zenity > /dev/null 2> /dev/null
    then
      yesno_question="zenity --question --text"
      pwd_prompt="zenity --password --text"
      infobox="zenity --info --text"
    else
      # Stilles Fehlschlagen, da wir den Fehler ja nicht anzeigen können.
      exit
    fi
}


function configure_display_and_user() {
    # Wenn das Skript via UDEV gestartet wird, ist der Nutzer root und das
    # Display nicht gesetzt. Daher müssen diese hier wild geraten werden. Bei
    # Systemen mit nur einem Benutzer sollte es aber keine Probleme geben. Wenn
    # das Skript jedoch von Hand gestartet wird, kann alles automatisch
    # bestimmt werden.
    if [[ "$start_via_udev" == true ]]
    then
      DISPLAY=:0; export DISPLAY
      curUser='#1000' #Nutzername oder NutzerID eintragen
    else
      curUser=$(who am i | awk '{print $1}') #ACHTUNG: 'who am i' kann nicht durch 'whoami' ersetzt werden!
    fi
}


function parse_cli_arguments() {
    if [[ $# -gt 3 ]]
    then
        echo "Skript mit zu vielen Argumente aufgerufen." >&2
        exit 1
    fi

    device=""
    ordnerliste=""
    while [[ $# -gt 0 ]]
    do
        local curArg
        curArg="$1"; shift
        case "$curArg" in
            -h|--help)
                echo "Hilfetext noch nicht geschrieben" >&2
                exit 1
                ;;
            -i|--interactive)
                interactive="true"
                ;;
            --no-interactive)
                interactive="false"
                ;;
            -*)
                echo "Unbekanntes Argument '$curArg'." >&2
                exit 1
                ;;
            *)
                if [[ -z "$device" ]]
                then
                    parse_device_arg "$curArg"
                elif [[ -z "$ordnerliste" ]]
                then
                    parse_ordnerliste_arg "$curArg"
                else
                    echo "Unerwartetes Argument '$curArg'." >&2
                    exit 1
                fi
        esac
    done
    if [[ -z "$device" ]]
    then
        echo "Kein Zielgerät für Backup angegeben!" >&2
        exit 1
    fi
    if [[ -z "$ordnerliste" ]]
    then
        ordnerliste="$basedir/ordnerliste"
    fi
}

function parse_device_arg() {
    deviceArg="$1"; shift
    if [[ -e "$deviceArg" ]]
    then
        device="$deviceArg"
    elif [[ -e "/dev/$deviceArg" ]]
    then
        device="/dev/$deviceArg";
    else
        misserfolg "Die Datei bzw. das Gerät, auf welche die Sicherungskopie gespielt werden soll, kann nicht gefunden werden."
    fi
}


function parse_ordnerliste_arg() {
    ordnerliste="$1"; shift
    if [[ ! -r "$ordnerliste" ]]
    then
        misserfolg "Die Liste der zu kopierenden Ordner ist nicht lesbar."
    fi
}


function decrypt_device() {
    if [[ -e $keyFileName ]]
    then
        decrypt_device_by_keyfile
    fi
    if [[ ! -e $keyFileName || $keyFileWorked -eq 2 ]]
    then
        decrypt_device_by_password
    fi
}


function decrypt_device_by_keyfile() {
    cryptsetup luksOpen "$device" "$mountDir" --key-file $keyFileName
    keyFileWorked=$?
    if [[ $keyFileWorked -eq 2 ]]
    then
        sudo -u "$curUser" "$infobox" "Das Backupziel kann mit der Schlüsseldatei $keyFileName nicht entschlüsselt werden. Bitte geben Sie das korrekte Passwort manuell ein."
    elif [[ $keyFileWorked -ne 0 ]]
    then
        misserfolg "Das Backupziel konnte nicht entschlüsselt werden. Der Fehlercode von cryptsetup ist $keyFileWorked."
    fi
}


function decrypt_device_by_password() {
    errmsg="Die Passworteingabe wurde abgebrochen. Die Erstellung der Sicherheitskopie kann daher nicht fortgesetzt werden."
    # shellcheck disable=SC2086
    # $pwd_prompt must be splitted
    if ! pwt=$(sudo -u "$curUser" $pwd_prompt "Bitte Passwort eingeben.")
    then
        misserfolg "$errmsg"
    fi

    while ! echo "$pwt" | cryptsetup luksOpen "$device" "$mountDir"
    do
        # shellcheck disable=SC2086
        # $pwd_prompt must be splitted
        if ! pwt=$(sudo -u "$curUser" $pwd_prompt "Das Passwort war falsch. Bitte nochmal eingeben!")
        then
          misserfolg "$errmsg"
        fi
    done
}


function mount_device() {
    fs_type=$(file -Ls "/dev/mapper/$mountDir" | grep -ioE '(btrfs|ext)')
    if [[ -z "$fs_type" ]]
    then
      misserfolg "Unbekanntes Dateisystem gefunden. Unterstützt werden nur 'ext' und 'btrfs'."
    fi

    fs_type_lc="${fs_type,,}" # in Kleinschreibung umwandeln
    if [[ "$fs_type_lc" == btrfs ]]
    then
      # Komprimierung mit ZLIB, da dies die kleinsten Dateien verspricht. Mit
      # ZSTD könnten noch höhere Komprimierungen erreicht werden, wenn ein
      # höheres Level gewählt werden könnte. Dies ist noch nicht der Fall.
      mount_opts="-o compress=zlib"
    fi
    mkdir "/media/$mountDir"
    # shellcheck disable=SC2086
    # $mount_opts must be splitted
    if ! mount $mount_opts "/dev/mapper/$mountDir" "/media/$mountDir"
    then
      misserfolg "Das Einbinden des Backupziels ist fehlgeschlagen."
    fi
}


function create_backup() {
    grep -v '^\s*#' "$ordnerliste" | while read -r line
    do
      orig=$(echo "$line" | cut -d ' ' -f1)/ # beachte abschließendes "/"!
      ziel=$(echo "$line" | cut -d ' ' -f2)
      prefix="/media/$mountDir/Sicherungskopien/$ziel/"
      curBackup="$prefix/${ziel}_$curDate"
      prevBackup=$(find "$prefix" -maxdepth 1 | sort | tail -n1)
      if [[ "$fs_type_lc" == btrfs ]]
      then
        cp -a --recursive --reflink=always "$prevBackup" "$curBackup"
        rsync -ax --delete --inplace "$orig" "$curBackup"
      else
        backup_dir="$(basename "$prevBackup")"
        rsync -ax --delete --link-dest="../$backup_dir" "$orig" "$curBackup"
      fi
    done
}

main "$@"
