;; (c) 2021 Sven Eusewig sveneusewig@yahoo.de
;; $Author: Zenlima $ $Date: 2011/12/10 01:30:00 $ $Revision: 1.34 $
;; $Author: chris $ $Date: 2009/07/29 09:31:44 $ $Revision: 1.33 $
;; Modified by Dmitry Smirnov <onlyjob@member.fsf.org>  16 Feb 2012
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
;; 02111-1307 USA

; If you want to adapt this report privately:
; - copy the report to your .gnucash directory
; - specify a different module name below (eg mytaxinvoice)
; - refer to it from .gnucash/config.user
; (see https://wiki.gnucash.org/wiki/Custom_Reports )
(define-module (gnucash reports standard rechnung))

(use-modules (ice-9 local-eval))  ; for the-environment
(use-modules (gnucash engine))
(use-modules (gnucash utilities))
(use-modules (gnucash core-utils))
(use-modules (gnucash app-utils))
(use-modules (gnucash report))
(use-modules (gnucash html))

(use-modules (gnucash eguile))

(use-modules (srfi srfi-13)) ; for extra string functions

(define delimiter " • ")

(define-public (nl->delimiter str)
  (string-substitute-alist str '((#\newline . ", "))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Report-specific routines

(define (taxrate taxable taxtable curr)
  ;; Display the tax rate applicable to an invoice line.
  ;; This may be e.g. "15%" or "£5.00" or "15% + £5.00" or "n/a"
  ;; depending on how complicated the tax table is.
  ;; (When called from within the eguile template, anything
  ;; (display)ed becomes part of the HTML string.)
  (cond
   ((or (not taxable) (eq? taxtable '()))
    (display "&nbsp;"))
   (else
    (let* ((amttot  (gnc:make-commodity-collector))
           (pctot   (gnc:make-value-collector))
           (entries (gncTaxTableGetEntries taxtable))
           (amt?    #f)  ; becomes #t if any entries are amounts
           (pc?     #f)) ; becomes #t if any entries are percentages
      (for-each
       (lambda (entry)
         (cond
          ((eqv? (gncTaxTableEntryGetType entry) GNC-AMT-TYPE-VALUE)
           (set! amt? #t)
           (amttot 'add curr (gncTaxTableEntryGetAmount entry)))
          (else
           (set! pc? #t)
           (pctot 'add (gncTaxTableEntryGetAmount entry)))))
       entries)
      (if pc? (format #t "~a%" (pctot 'total #f)))
      (if (and amt? pc?) (display " +&nbsp;"))
      (if amt? (display-comm-coll-total amttot #f))
      (if (equal? amt? pc? #f) (display (G_ "n/a")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Define all the options

; option pages
(define headingpage  (N_ "Headings 1"))
(define headingpage2 (N_ "Headings 2"))
(define notespage    (N_ "Notes"))
(define extracsspage    (N_ "Extra CSS"))
(define displaypage  (N_ "Display"))
(define elementspage			(N_ "Elements"))
(define companyaddpage      (N_ "Company"))
(define bankconnectionpage  (N_ "Bank connection"))
; option names 
(define optname-col-date		(N_ "column: Date"))
(define optname-col-taxrate		(N_ "column: Tax Rate"))
(define optname-col-units		(N_ "column: Units"))
(define optname-row-address		(N_ "row: Address"))
(define optname-row-contact		(N_ "row: Contact"))
(define optname-row-invoice-number	(N_ "row: Invoice Number"))
(define optname-row-company-name	(N_ "row: Company Name"))
(define optname-invoice-number-text	(N_ "Invoice number text"))
(define optname-to-text			(N_ "To text"))
(define optname-ref-text		(N_ "Ref text"))
(define optname-jobname-text		(N_ "Job Name text"))
(define optname-jobnumber-text		(N_ "Job Number text"))
(define optname-jobname-show		(N_ "Show Job name"))
(define optname-jobnumber-show		(N_ "Show Job number"))
(define optname-netprice		(N_ "Show net price"))
(define optname-returnaddress    (N_ "Show return address"))
(define optname-invnum-next-to-title	(N_ "Invoice number next to title"))
(define optname-border-collapse		(N_ "table-border-collapse"))
(define optname-border-color-th		(N_ "table-header-border-color"))
(define optname-border-color-td		(N_ "table-cell-border-color"))
(define optname-extra-css		(N_ "Embedded CSS"))
(define optname-report-title		(N_ "Report title"))
(define optname-template-file		(N_ "Template file"))
(define optname-heading-font		(N_ "Heading font"))
(define optname-text-font		(N_ "Text font"))
(define optname-logofile	        (N_ "Logo filename"))
(define optname-logo-width     		(N_ "Logo width"))
(define optname-units          		(N_ "Units"))
(define optname-qty            		(N_ "Qty"))
(define optname-unit-price     		(N_ "Unit Price"))
(define optname-disc-rate      		(N_ "Discount Rate"))
(define optname-disc-amount    		(N_ "Discount Amount"))
(define optname-net-price      		(N_ "Net Price"))
(define optname-tax-rate       		(N_ "Tax Rate"))
(define optname-tax-amount     		(N_ "Tax Amount"))
(define optname-total-price    		(N_ "Total Price"))
(define optname-subtotal       		(N_ "Sub-total"))
(define optname-amount-due     		(N_ "Amount Due"))
(define optname-payment-recd   		(N_ "Payment received text"))
(define optname-extra-notes    		(N_ "Extra notes"))
(define optname-extra-notes-before (N_ "Extra notes before table"))

;; German additional Fields
(define optname-company-slogan            (N_ "Company slogan"))
(define optname-coyid-title               (N_ "Coyid text"))
(define optname-taxid               (N_ "Steuernummer/UStId text"))

(define optname-bank-connection-title     (N_ "Bankverbindung Text"))
(define optname-bank-name-title           (N_ "Bank name text"))
(define optname-bank-nationalcode-title   (N_ "National bank code text"))
(define optname-bank-swiftcode-title      (N_ "Swift BIC text"))
(define optname-bank-ibancode-title       (N_ "IBAN text"))
(define optname-bank-accountnumber-title  (N_ "Account number text"))
(define optname-bank-name                 (N_ "Bank name"))
(define optname-bank-nationalcode         (N_ "National bank code"))
(define optname-bank-swiftcode            (N_ "Swift BIC"))
(define optname-bank-ibancode             (N_ "IBAN"))
(define optname-bank-accountnumber        (N_ "Account number"))
(define optname-returnaddress             (N_ "Show return address"))
;; End German additional Fields

;; Define new option names
(define optname-kleinunternehmerregelung (N_ "Kleinunternehmerregelung"))
(define optname-leistungsdatum (N_ "Leistungsdatum definieren"))

(define (options-generator)
  ;; Options
  (define report-options (gnc:new-options))
  (define (add-option new-option)
    (gnc:register-option report-options new-option))

  (add-option
    (gnc:make-invoice-option ; defined in gnucash/scm/business-options.scm
      gnc:pagename-general gnc:optname-invoice-number 
      "a" "" (lambda () '()) #f))

  ;; Elements page options
(add-option (gnc:make-simple-boolean-option	elementspage	optname-col-date		"a" (N_ "Display the date?") #t))
(add-option (gnc:make-simple-boolean-option	elementspage	optname-col-taxrate		"b" (N_ "Display the Tax Rate?") #t))
(add-option (gnc:make-simple-boolean-option	elementspage	optname-col-units		"c" (N_ "Display the Units?") #t))
(add-option (gnc:make-simple-boolean-option	elementspage	optname-row-contact		"d" (N_ "Display the contact?") #t))
(add-option (gnc:make-simple-boolean-option	elementspage	optname-row-address		"e" (N_ "Display the address?") #t))
(add-option (gnc:make-simple-boolean-option	elementspage	optname-row-invoice-number	"f" (N_ "Display the Invoice Number?") #t))
(add-option (gnc:make-simple-boolean-option	elementspage	optname-row-company-name	"g" (N_ "Display the Company Name?") #t))
(add-option (gnc:make-simple-boolean-option	elementspage	optname-invnum-next-to-title	"h" (N_ "Invoice Number next to title?") #f))
(add-option (gnc:make-simple-boolean-option	elementspage	optname-jobname-show		"i" (N_ "Display Job name?") #t))
(add-option (gnc:make-simple-boolean-option	elementspage	optname-jobnumber-show		"j" (N_ "Invoice Job number?") #f))
(add-option (gnc:make-simple-boolean-option	elementspage	optname-netprice		"k" (N_ "Show net price?") #f))
(add-option (gnc:make-simple-boolean-option elementspage  optname-returnaddress    "l" (N_ "Show return address?") #f))
(add-option (gnc:make-simple-boolean-option elementspage optname-kleinunternehmerregelung "m" (N_ "Display Kleinunternehmerregelung?") #f))
(add-option (gnc:make-string-option elementspage optname-leistungsdatum "n" (N_ "Specify Leistungsdatum (date)") ""))

  ;; Display options
  (add-option (gnc:make-string-option displaypage optname-template-file "a" 
    (N_ "The file name of the eguile template part of this report. This file should either be in your .gnucash directory, or else in its proper place within the GnuCash installation directories.")
    "rechnung.eguile.scm"))
  (add-option (gnc:make-font-option 
                displaypage optname-heading-font "c" 
                (N_ "Font to use for the main heading.") "Sans Bold 18"))
  (add-option (gnc:make-font-option 
                displaypage optname-text-font "d" 
                (N_ "Font to use for everything else.") "Sans 10"))
  (add-option (gnc:make-pixmap-option
                displaypage optname-logofile "e" 
                (N_ "Name of a file containing a logo to be used on the report.") 
                ""))
  (add-option (gnc:make-string-option
                displaypage optname-logo-width "f" (N_ "Width of the logo in CSS format, e.g. 10% or 32px. Leave blank to display the logo at its natural width. The height of the logo will be scaled accordingly.") ""))
(add-option (gnc:make-simple-boolean-option	displaypage	optname-border-collapse	"g" (N_ "Border-collapse?") #f))
(add-option (gnc:make-string-option		displaypage	optname-border-color-th "h" (N_ "CSS color.") "black"))
(add-option (gnc:make-string-option		displaypage	optname-border-color-td "i" (N_ "CSS color.") "black"))

  ;; Heading options
  (add-option (gnc:make-string-option
                ; page / name / orderkey / tooltip / default
                headingpage optname-report-title "a" "" (G_ "Rechnung")))
  (add-option (gnc:make-string-option
                headingpage optname-units "b" "" (G_ "Einheit")))
  (add-option (gnc:make-string-option
                headingpage optname-qty "c" "" (G_ "Menge")))
  (add-option (gnc:make-string-option
                headingpage optname-unit-price "d" "" (G_ "Einzelpreis")))
  (add-option (gnc:make-string-option
                headingpage optname-disc-rate "e" "" (G_ "Rabattanteil")))
  (add-option (gnc:make-string-option
                headingpage optname-disc-amount "f" "" (G_ "Rabatthöhe")))
  (add-option (gnc:make-string-option
                headingpage optname-net-price "g" "" (G_ "Nettopreis")))
  (add-option (gnc:make-string-option
                headingpage optname-tax-rate "h" "" (G_ "USt %")))
  (add-option (gnc:make-string-option
                headingpage optname-tax-amount "i" "" (G_ "USt")))
  (add-option (gnc:make-string-option
                headingpage optname-total-price "j" "" (G_ "Gesamtpreis")))
  (add-option (gnc:make-string-option
                headingpage2 optname-subtotal "a" "" (G_ "Zwischhensumme")))
  (add-option (gnc:make-string-option
                headingpage2 optname-amount-due "b" "" (G_ "Gesamtbetrag")))
  (add-option (gnc:make-string-option
                headingpage2 optname-payment-recd "c" "" 
                (G_ "Payment received, thank you!")))
  (add-option (gnc:make-string-option	headingpage2	optname-invoice-number-text
    "d" "" (G_ "Invoice number: ")))
  (add-option (gnc:make-string-option	headingpage2	optname-to-text
    "e" "" (G_ "To: ")))
  (add-option (gnc:make-string-option	headingpage2	optname-ref-text
    "f" "" (G_ "Your ref: ")))
  (add-option (gnc:make-string-option	headingpage2	optname-jobnumber-text
    "g" "" (G_ "Job number: ")))
  (add-option (gnc:make-string-option	headingpage2	optname-jobname-text
    "h" "" (G_ "Job name: ")))

  (add-option (gnc:make-text-option
                notespage optname-extra-notes-before "a"
                (G_ "Notes added before invoice -- may contain HTML markup.")
                ""))
  (add-option (gnc:make-text-option
                notespage optname-extra-notes "b"
                (G_ "Notes added at end of invoice -- may contain HTML markup.") 
                (G_ "Thank you for your patronage!")))

  (add-option (gnc:make-text-option	extracsspage optname-extra-css "a"
                (N_ "Embedded CSS.")	"h1.coyname { text-align: left; }"))
  (gnc:options-set-default-section
    report-options gnc:pagename-general)

  ;; Company additional options
  (add-option (gnc:make-string-option
                companyaddpage optname-company-slogan "a" "" ""))

  ;; Legal options
  (add-option (gnc:make-string-option
                companyaddpage optname-coyid-title "e" "" ""))
  (add-option (gnc:make-string-option
                companyaddpage optname-taxid "e" "" ""))

  ;; Bank connection options
  (add-option (gnc:make-string-option
                bankconnectionpage optname-bank-connection-title "a" "" ""))
  (add-option (gnc:make-string-option
                bankconnectionpage optname-bank-name-title "b" "" ""))
  (add-option (gnc:make-string-option
                bankconnectionpage optname-bank-name "c" "" ""))
  (add-option (gnc:make-string-option
                bankconnectionpage optname-bank-nationalcode-title "d" "" ""))
  (add-option (gnc:make-string-option
                bankconnectionpage optname-bank-nationalcode "e" "" ""))
  (add-option (gnc:make-string-option
                bankconnectionpage optname-bank-swiftcode-title "f" "" ""))
  (add-option (gnc:make-string-option
                bankconnectionpage optname-bank-swiftcode "g" "" ""))
  (add-option (gnc:make-string-option
                bankconnectionpage optname-bank-accountnumber-title "h" "" ""))
  (add-option (gnc:make-string-option
                bankconnectionpage optname-bank-accountnumber "i" "" ""))
  (add-option (gnc:make-string-option
                bankconnectionpage optname-bank-ibancode-title "j" "" ""))
  (add-option (gnc:make-string-option
                bankconnectionpage optname-bank-ibancode "k" "" ""))

  report-options)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Create the report

(define (report-renderer report-obj)
  ;; Create and return the report as either an HTML string 
  ;; or an <html-document>
  (define (opt-value section name)
    ; wrapper for option routines
    (define (get-opt section name)
      (gnc:lookup-option (gnc:report-options report-obj) section name))
    (gnc:option-value (get-opt section name)))

  ; Get all the options
  (let* ((document                  (gnc:make-html-document))
         (opt-invoice               (opt-value gnc:pagename-general gnc:optname-invoice-number))
         (opt-template-file         (find-template
                                      (opt-value displaypage optname-template-file)))
         (opt-heading-font          (font-name-to-style-info 
                                      (opt-value displaypage optname-heading-font)))
         (opt-text-font             (font-name-to-style-info
                                      (opt-value displaypage optname-text-font)))
         (opt-logofile              (opt-value displaypage  optname-logofile)) 
         (opt-logo-width            (opt-value displaypage  optname-logo-width)) 
         (opt-col-date              (opt-value elementspage  optname-col-date))
         (opt-col-taxrate           (opt-value elementspage  optname-col-taxrate))
         (opt-col-units             (opt-value elementspage  optname-col-units))
         (opt-row-contact           (opt-value elementspage  optname-row-contact))
         (opt-row-address           (opt-value elementspage  optname-row-address))
         (opt-row-invoice-number    (opt-value elementspage  optname-row-invoice-number))
         (opt-row-company-name      (opt-value elementspage  optname-row-company-name))
         (opt-invnum-next-to-title  (opt-value elementspage  optname-invnum-next-to-title))
         (opt-jobname-show          (opt-value elementspage  optname-jobname-show))
         (opt-jobnumber-show        (opt-value elementspage  optname-jobnumber-show))
         (opt-netprice              (opt-value elementspage  optname-netprice))
         (opt-returnaddress         (opt-value elementspage  optname-returnaddress))
         (opt-kleinunternehmerregelung (opt-value elementspage optname-kleinunternehmerregelung))
         (opt-leistungsdatum (opt-value elementspage optname-leistungsdatum))

         (opt-invoice-currency      (gncInvoiceGetCurrency opt-invoice))
         (opt-css-border-collapse   (if (opt-value displaypage optname-border-collapse) "border-collapse:collapse;"))
         (opt-css-border-color-th   (opt-value displaypage optname-border-color-th))
         (opt-css-border-color-td   (opt-value displaypage optname-border-color-td))
         (opt-report-title          (opt-value headingpage  optname-report-title))
         (opt-units-heading         (opt-value headingpage  optname-units))
         (opt-qty-heading           (opt-value headingpage  optname-qty))
         (opt-unit-price-heading    (opt-value headingpage  optname-unit-price))
         (opt-disc-rate-heading     (opt-value headingpage  optname-disc-rate))
         (opt-disc-amount-heading   (opt-value headingpage  optname-disc-amount))
         (opt-net-price-heading     (opt-value headingpage  optname-net-price))
         (opt-tax-rate-heading      (opt-value headingpage  optname-tax-rate))
         (opt-tax-amount-heading    (opt-value headingpage  optname-tax-amount))
         (opt-total-price-heading   (opt-value headingpage  optname-total-price))
         (opt-subtotal-heading      (opt-value headingpage2 optname-subtotal))
         (opt-amount-due-heading    (opt-value headingpage2 optname-amount-due))
         (opt-payment-recd-heading  (opt-value headingpage2 optname-payment-recd))
         (opt-invoice-number-text   (opt-value headingpage2 optname-invoice-number-text))
         (opt-to-text               (opt-value headingpage2 optname-to-text))
         (opt-ref-text              (opt-value headingpage2 optname-ref-text))
         (opt-jobnumber-text        (opt-value headingpage2 optname-jobnumber-text))
         (opt-jobname-text          (opt-value headingpage2 optname-jobname-text))
         (opt-extra-css             (opt-value extracsspage    optname-extra-css)) 
         (opt-extra-notes           (opt-value notespage    optname-extra-notes))
         (opt-extra-notes-before    (opt-value notespage    optname-extra-notes-before))
         (opt-company-slogan            (opt-value companyaddpage optname-company-slogan))
         (optname-coyid-title           (opt-value companyaddpage optname-coyid-title))
         (opt-taxid           (opt-value companyaddpage optname-taxid))  
         (opt-bank-connection-title     (opt-value bankconnectionpage optname-bank-connection-title))
         (opt-bank-name-title           (opt-value bankconnectionpage optname-bank-name-title))
         (opt-bank-name                 (opt-value bankconnectionpage optname-bank-name))
         (opt-bank-nationalcode-title   (opt-value bankconnectionpage optname-bank-nationalcode-title))
         (opt-bank-nationalcode         (opt-value bankconnectionpage optname-bank-nationalcode))
         (opt-bank-swiftcode-title      (opt-value bankconnectionpage optname-bank-swiftcode-title))
         (opt-bank-swiftcode            (opt-value bankconnectionpage optname-bank-swiftcode))
         (opt-bank-accountnumber-title  (opt-value bankconnectionpage optname-bank-accountnumber-title))
         (opt-bank-accountnumber        (opt-value bankconnectionpage optname-bank-accountnumber))
         (opt-bank-ibancode-title       (opt-value bankconnectionpage optname-bank-ibancode-title))
         (opt-bank-ibancode             (opt-value bankconnectionpage optname-bank-ibancode))
         (html (eguile-file-to-string
                 opt-template-file
                 (the-environment))))

    (gnc:debug "rechnung_dbg.scm - generated html:") (gnc:debug html)

    html))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Define the report

(gnc:define-report
  'version 1
  'name (N_ "Rechnung")
  'report-guid "246628e721744131bf53d91bb90cd336"
  'menu-name (N_ "Rechnung")
  'menu-tip (N_ "Deutsche Rechnung erstellen")
  'menu-path (list gnc:menuname-business-reports)
  'options-generator options-generator
  'renderer report-renderer)

(define (au-tax-options-generator)
  (define (set-opt options page name value)
    (let ((option (gnc:lookup-option options page name)))
         (gnc:option-set-value option value)))

  (let ((options (options-generator)))
       (set-opt options headingpage optname-report-title (G_ "Rechnung"))
       (set-opt options headingpage optname-unit-price (G_ "Einheit"))
       (set-opt options headingpage optname-tax-rate (G_ "USt %"))
       (set-opt options headingpage optname-tax-amount (G_ "USt"))
       (set-opt options headingpage2 optname-amount-due (G_ "Gesamtbetrag (inkl. USt)"))
       (set-opt options headingpage2 optname-invoice-number-text (G_ "Rechnung #: "))
       (set-opt options headingpage2 optname-ref-text (G_ "Referenz: "))
       (set-opt options headingpage2 optname-jobname-text (G_ "Bestellung: "))
       (set-opt options extracsspage optname-extra-css "h1.coyname { text-align: right; margin-bottom: 0px ; font-size: 200%; } h2.invoice { text-align: left; margin-bottom: 0px ; font-size: 500%; }")
       options))
