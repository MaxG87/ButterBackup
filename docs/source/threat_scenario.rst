Gefährdungsszenario
===================

ButterBackup wurde entworfen, um gegen eine Reihe spezieller
Gefährdungsszenarien zu schützen. Die konkreten Bedrohungen und die
entsprechenden Gegenmaßnahmen gibt die folgende Tabelle an. Auf die
Übersichtstabelle folgt eine etwas ausführlichere Diskussion der Maßnahmen.

| Bedrohung                                          | Gegenmaßnahme                        |
| -------------------------------------------------- | ------------------------------------ |
| Dateien verschlüsselnde Schadsoftware              | physikalisch getrennte Aufbewahrung  |
| fehlerhafte Sicherungskopien durch Fehlbenutzung   | sehr einfache Benutzung              |
| Verlust des Datenträgers                           | Vollverschlüsselung des Datenträgers |
| Zerstörung des Datenträgers durch Spannungsspitzen | physikalisch getrennte Aufbewahrung  |
| zu seltene Sicherungskopien                        | sehr einfache Benutzung              |

Diese Bedrohungen werden dadurch reduziert, dass eine sehr einfache Benutzung
die physikalisch getrennte Aufbewahrung des die Sicherungskopien enthaltenen
Datenträgers ermöglicht.

Eine einfach umzusetzende und verlässliche Maßnahme, um den Datenträger
vor Zerstörung durch Spannungsspitzen zu schützen, ist, ihn vom Computer
physikalisch getrennt aufzubewahren. Gleichzeitig schützt diese Maßnahme
auch sehr gut vor der Zerstörung der Sicherungskopien durch Schadsoftware, da
sich die Sicherungskopien außerhalb des Zugriffs der Schadsoftware befinden.

Durch die physikalisch getrennte Aufbewahrung ergibt sich aber eine Bedrohung
durch Bequemlichkeit. Ein Prozess, für dessen Durchführung manuelle
Schritte nötig sind, ist fehleranfällig und läuft Gefahr, im Zweifel
nicht ausgeführt zu werden. Das Anlegen von Sicherungskopien stellt hier
keine Ausnahme dar.

ButterBackup begegnet dieser Gefahr dadurch, dass es das Anlegen einer
Sicherungskopie auf zwei manuelle Schritte reduziert. Es genügt, die
Festplatte mit dem Computer zu verbinden und das Programm zu starten. Alle
weiteren Schritte, z.B. Entschlüsselung, Kopie der Daten und Unmounten,
werden von ButterBackup übernommen.

Insgesamt ermöglicht ButterBackup, Sicherungskopien so sicher wie möglich
aufzubewahren ohne dabei zu verkomplizieren, neue Sicherungskopien anzulegen.
