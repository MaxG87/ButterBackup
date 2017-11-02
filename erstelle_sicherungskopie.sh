#!/bin/bash
#Übernimmt als Parameter den Gerätenamen, z.B. 'sdb1'.

function misserfolg {
  sudo -u "$curUser" $infobox "$@"
  if [[ ! -z "$mountDir" ]]
  then
    aufraeumen
    if [[ -e "/media/$mountDir" ]]
    then
      del_str="\nDer Ordner \"/media/$mountDir\" muss manuell gelöscht werden."
    fi
    if [[ -e "/dev/mapper/$mountDir" ]]
    then
      del_str="$del_str\nDas Backupziel konnte nicht sauber entfernt werden. Die Entschlüsselung in \"/dev/mapper/$mountDir\" muss daher manuell gelöst werden."
    fi
    if [[ ! -z "$delStr" ]]
    then
      sudo -u "$curUser" $infobox $del_str
    fi
  fi
  exit
}

function aufraeumen {
  [[ -e "/media/$mountDir" ]]      && umount "/media/$mountDir"
  [[ -e "/dev/mapper/$mountDir" ]] && cryptsetup close "$mountDir"
  [[ -e "/media/$mountDir" ]]      && rmdir "/media/$mountDir"
}

#Wir betreten die Hölle der Platformabhängigkeit.
#Auf KDE-Systemen kann zenity nicht vorausgesetzt werden, auf GNOME-Systemen
#hingegen kdialog nicht. Daher muss der entsprechende Befehl zur Laufzeit
#bestimmt werden, um nicht immer zwei sehr ähnliche Skripte pflegen zu müssen.
if type kdialog > /dev/null 2> /dev/null
then
  dialog_type=kdialog
  yesno_question="kdialog --yesno"
  pwd_prompt="kdialog --password"
  infobox="kdialog --msgbox"
elif type zenity > /dev/null 2> /dev/null
then
  dialog_type=zenity
  yesno_question="zenity --question --text"
  pwd_prompt="zenity --password --text"
  infobox="zenity --info --text"
else
  #Stilles Fehlschlagen, da wir den Fehler ja nicht anzeigen können.
  exit
fi

#Wenn das Skript via UDEV gestartet wird, ist der Nutzer root und das Display
#nicht gesetzt. Daher müssen diese hier wild geraten werden. Bei Systemen mit
#nur einem Benutzer sollte es aber keine Probleme geben.
#Wenn das Skript jedoch von Hand gestartet wird, kann alles automatisch
#bestimmt werden.
start_via_udev="false"
if [[ "$start_via_udev" == true ]]
then
  DISPLAY=:0; export DISPLAY
  curUser='#1000' #Nutzername oder NutzerID eintragen
else
  curUser=$(who am i | awk '{print $1}') #ACHTUNG: 'who am i' kann nicht durch 'whoami' ersetzt werden!
fi

if ! sudo -u "$curUser" $yesno_question "Soll eine Sicherungskopie erstellt werden?"
then
  exit
fi

#Validierung der Übergabeparameter
if [[ -e "$1" ]]
then
  device="$1"
elif [[ -e "/dev/$1" ]]
then
  device="/dev/$1";
else
  misserfolg "Die Datei bzw. das Gerät, auf welche die Sicherungskopie gespielt werden soll, kann nicht gefunden werden."
fi

#Öffne Gerät
keyFileName=/opt/Sicherungskopien/keyfile_extern
mountDir=$(date +%s)
if [[ -e $keyFileName ]]
then
  cryptsetup luksOpen "$device" "$mountDir" --key-file $keyFileName
  keyFileWorked=$?
  if [[ $keyFileWorked -eq 2 ]]
  then
    sudo -u "$curUser" $infobox "Das Backupziel kann mit der Schlüsseldatei $keyFileName nicht entschlüsselt werden. Bitte geben Sie das korrekte Passwort manuell ein."
  elif [[ $keyFileWorked -ne 0 ]]
  then
    misserfolg "Das Backupziel konnte nicht entschlüsselt werden. Der Fehlercode von cryptsetup ist $keyFileWorked."
  fi
fi
if [[ ! -e $keyFileName || $keyFileWorked -eq 2 ]]
then
  pwt=$(sudo -u "$curUser" $pwd_prompt "Bitte Passwort eingeben.")
  if [[ $? -ne 0 ]]
  then
    misserfolg "Die Passworteingabe wurde abgebrochen. Die Erstellung der Sicherheitskopie kann daher nicht fortgesetzt werden."
  fi

  while ! echo "$pwt" | cryptsetup luksOpen "$device" "$mountDir" 
  do
    pwt=$(sudo -u "$curUser" $pwd_prompt "Das Passwort war falsch. Bitte nochmal eingeben!")
    if [[ $? -ne 0 ]]
    then
      misserfolg "Die Passworteingabe wurde abgebrochen. Die Erstellung der Sicherheitskopie kann daher nicht fortgesetzt werden."
    fi
  done
fi

#Mounten
fs_type=$(file -Ls /dev/mapper/$mountDir | grep -ioE '(btrfs|ext)')
if [[ "$fs_type" == "" ]]
then
  misserfolg "Unbekanntes Dateisystem gefunden. Unterstützt werden nur 'ext' und 'btrfs'."
fi

fs_type_lc="${fs_type,,}" # in Kleinschreibung umwandeln
if [[ "$fs_type_lc" == btrfs ]]
then
  mount_opts="-o compress=zlib"
fi
mkdir "/media/$mountDir"
if ! mount $mount_opts "/dev/mapper/$mountDir" "/media/$mountDir"
then
  misserfolg "Das Einbinden des Backupziels ist fehlgeschlagen."
fi

#Erstelle Sicherungskopien
curDate=$(date +%F_%H%M)
basedir=$(dirname "$0")
if [[ -f "$basedir/ordnerliste" ]]
then
  ordnerliste="$basedir/ordnerliste"
elif [[ -f "$2" ]]
then
  ordnerliste="$2"
else
  misserfolg "Die Liste der zu kopierenden Ordner konnte nicht gefunden werden."
fi

grep -v '^\s*#' "$ordnerliste" | while read line
do
  orig=$(echo $line | cut -d ' ' -f1)/ # beachte abschließendes "/"!
  ziel=$(echo $line | cut -d ' ' -f2)
  prefix="/media/$mountDir/Sicherungskopien/$ziel/"
  curBackup="$prefix/${ziel}_$curDate"
  if [[ "$fs_type_lc" == btrfs ]]
  then
    prevBackup=$(find "$prefix" -maxdepth 1 | sort | tail -n1)
    cp --recursive --reflink=always "$prevBackup" "$curBackup"
    rsync -a --delete --inplace "$orig" "$curBackup"
  else
    prevBackup=$(ls "$prefix" | tail -n1)
    rsync -a --delete --link-dest="../$prevBackup" "$orig" "$curBackup"
  fi
done

#Aufräumen
aufraeumen

sudo -u "$curUser" $infobox "Eine Sicherungskopie wurde erfolgreich angelegt.";
