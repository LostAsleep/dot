;;; package --- Autotexte Radiologiebefunde

;;; Commentary:
;;; Eine Sammlung von den Standarbefunden die ich regelmäßig benutze.

;;; Code:
(defun phi-dkgon ()
  "Autotext Ganzbeinaufnahme im Stand."
  (interactive)
  (insert "Die Miculicz-Linie schneidet das mediale / laterale Tibiaplateau xxx cm medial / lateral der Area intercondylaris. Der lateral gemessene offene Winkel zwischen Femurschaft- und Tibiaschaftachse beträgt ca. ...° (normal ca. 169-178°). Daraus ergibt sich eine Varusstellung (>178°) / Valgusstellung (<169°) im rechten / linken Kniegelenk."))


(defun phi-ct-thorax-normalbefund ()
  "Autotext Normalbefund CT-Thorax."
  (interactive)
  (insert "Thorax:\nUnauffälliger Weichteilmantel ohne axilläre Lymphadenopathie. Mediastinum mittelständig. Herz nicht wesentlich vergrößert. Keine zentrale LAE. Keine hiläre oder mediastinale Lymphadenopathie. Regelrechter Verlauf der supraaortalen Gefäße. Kein Pleuraerguss. Keine Konsolidierungen, interstitielle Zeichnungsvermehrung oder malignitätssuspekten pulmonalen Rundherde."))


(defun phi-ct-abdomen-normalbefund ()
  "Autotext Normalbefund CT-Abdomen."
  (interactive)
  (insert "Abdomen:\nAbdomineller Weichteilmantel unauffällig. Homogenes Leberparenchym ohne Nachweis fokaler Läsionen. Keine Cholestase. Gallenblase, Pankreas, Milz, Nebennieren, Nieren in Lage, Form und Größe regelrecht mit unauffälligen KM-Verhalten. Kein Harnstau. An Magen, Dünndarm, Colonrahmen, und Rektum keine erkennbaren Wandverdickungen und Stenosen. Harnblase unauffällig. Unauffälliger Uterus und Adnexe ///// Prostata und Samenbläschen. Keine freie Luft oder freie Flüssigkeit. Keine pathologisch vergrößerten LK zwischen Zwerchfell und Symphyse. Unauffällige Darstellung der Gefäße (soweit in portalvenöser KM-Phase beurteilbar)."))


(defun phi-rö-thorax-liegend-normalbefund ()
  "Autotext Normalbefund Röntgen Thorax Liegendaufnahme."
  (interactive)
  (insert "Im Liegen gestauchte kardiomediastinale Silhouette, etwa mittelständig. Keine Lungenstauung. Keine Ergüsse. Keine flächigen Infiltrate in den einsehbaren Lungenabschnitten. Im Liegen ist kein Pneumothorax abgrenzbar. Annähernd symmetrischer Rippenthorax. Weichteilschatten unauffällig."))


(defun phi-keine-voraufnahmen ()
  "Autotext keine Voraufnahmen vorliegend."
  (interactive)
  (insert "Diesbezüglich liegen keine Voraufnahmen zum Vergleich vor."))


(defun phi-rö-osg-normalbefund ()
  "Autotext Normalbefund Röntgen oberes Sprunggelenk."
  (interactive)
  (insert "Unauffällige Stellung im OSG, USG und Chopart-Gelenk. Keine signifikanten degenerativen Veränderungen. Kein Hinweis auf frische Frakturen. Miterfasste Weichteile unauffällig."))


(defun phi-rö-keine-fraktur ()
  "Autotext Röntgen keine Fraktur."
  (interactive)
  (insert "Kortikale Strukturen erhalten, regelrechter Kalksalzgehalt – kein Frakturnachweis. Weichteilschatten unauffällig."))


(defun phi-rö-lws-normalbefund ()
  "Autotext Normalbefund Röntgen LWS / Lendenwirbelsäule."
  (interactive)
  (insert "Keine signifikante Skoliose. Harmonische LWS-Lordose. Harmonisches Hinterkantenalignement. Intakte Deck- und Grundplatten. Bandscheibenfächer normal hoch. Keine Osteochondrose oder Spondylose. Keine signifikanten Spondylarthrosezeichen. ISG beidseits blande. Miterfasste Weichteile unauffällig."))


(defun phi-petct-normalbefund ()
  "Autotext Normalbefund PET/CT CT-Teil"
  (interactive)
  (insert "Kopf/Hals: Keine zerebralen Läsionen, soweit nativ lowdose beurteilbar. Nasennebenhöhlen unauffällig. Aufhärtungsartefakte durch Zahnprothesenmaterial. Keine vergrößerten zervikalen LK.\n\nThorax: Keine malignitätssuspekte mediastinale, hiläre oder axilläre Lymphadenopathie. Keine intrapulmonalen Rundherde in Atemmittellage abgrenzbar. Keine flächigen pulmonalen Konsolidierungen.\n\nAbdomen: Regelrechte Abbildung der intraabdominellen Organe. Keine malignitätssuspekte abdominelle Lymphadenopathie. Keine freie Flüssigkeit. Keine freie Luft.\n\nExtremitäten / Skelett: Unauffälliger Weichteilmantel. Keine ossären Destruktionen."))


(defun phi-new-pat-demo ()
  "Insert new patient for demonstration."
  (interactive)
  (insert "** \n*** Klinik/Frage\n\n*** \n\n-----"))


(defun phi-test-baustin ()
  "Some Text"
  (interactive)
  (insert "blablablablabla"))


(provide 'rad-autotext)
;;; rad-autotext.el ends here
