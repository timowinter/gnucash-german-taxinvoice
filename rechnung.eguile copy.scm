<!DOCTYPE html>
<?scm
(let ((x 42)) ; only here to allow (define)s
  (define (display-report opt-invoice)

    (define (date<? s1 s2)
      (< (xaccTransGetDate (xaccSplitGetParent s1))
         (xaccTransGetDate (xaccSplitGetParent s2))))

    ;; Main function that creates the tax invoice report
    (let* (; invoice and company details
           (invoiceid    (gncInvoiceGetID         opt-invoice))
           (credit-note? (gncInvoiceGetIsCreditNote opt-invoice))
           (book         (gncInvoiceGetBook       opt-invoice))
           (isposted     (gncInvoiceIsPosted      opt-invoice))
           (postdate     (gncInvoiceGetDatePosted opt-invoice))
           (duedate      (gncInvoiceGetDateDue    opt-invoice))
           (billingid    (gncInvoiceGetBillingID  opt-invoice))
           (notes        (gncInvoiceGetNotes      opt-invoice))
           (terms        (gncInvoiceGetTerms      opt-invoice))
           (termsdesc    (gncBillTermGetDescription terms))
           (lot          (gncInvoiceGetPostedLot  opt-invoice))
           (txn          (gncInvoiceGetPostedTxn  opt-invoice))
           (currency     (gncInvoiceGetCurrency   opt-invoice))
           (entries      (gncInvoiceGetEntries    opt-invoice))
           (splits       (sort (gnc-lot-get-split-list lot) date<?))
           (dateformat   (gnc:options-fancy-date book))
           (coyname      (gnc:company-info book gnc:*company-name*))
           (coycontact   (gnc:company-info book gnc:*company-contact*))
           (coyaddr      (gnc:company-info book gnc:*company-addy*))
           (coyid        (gnc:company-info book gnc:*company-id*))
           (coyphone     (gnc:company-info book gnc:*company-phone*))
           (coyfax       (gnc:company-info book gnc:*company-fax*))
           (coyurl       (gnc:company-info book gnc:*company-url*))
           (coyemail     (gnc:company-info book gnc:*company-email*))
           (owner        (gncInvoiceGetOwner  opt-invoice))
           (owneraddr    (gnc:owner-get-address-dep owner))
           (ownername    (gnc:owner-get-name-dep owner))
           (jobnumber     (gncJobGetID (gncOwnerGetJob (gncInvoiceGetOwner  opt-invoice))))
           (jobname      (gncJobGetName (gncOwnerGetJob (gncInvoiceGetOwner  opt-invoice))))
           (billcontact   (gncAddressGetName (gnc:owner-get-address owner)))
           (cust-doc?    (eqv? (gncInvoiceGetType opt-invoice) GNC-INVOICE-CUST-INVOICE))
           (reverse-payments? (not (gncInvoiceAmountPositive opt-invoice)))
           ; flags and counters
           (discount?    #f) ; any discounts on this invoice?
           (tax?         #f) ; any taxable entries on this invoice?
           (payments?    #f) ; have any payments been made on this invoice?
           (units?       #f) ; does any row specify units?
           (qty?         #f) ; does any row have qty <> 1?
           (tbl_cols     0)) ; number of columns for 'colspan' attributes
      
      ;; pre-scan invoice entries to look for discounts and taxes
      (for-each
       (lambda (entry)
         (unless (string-null? (gncEntryGetAction entry)) (set! units? #t))
         (unless (= 1 (gncEntryGetDocQuantity entry credit-note?)) (set! qty? #t))
         (cond
          (cust-doc?
           (unless (zero? (gncEntryGetInvDiscount entry)) (set! discount? #t))
           (unless (null? (gncEntryGetInvTaxTable entry)) (set! tax? #t)))
          (else
           (unless (null? (gncEntryGetBillTaxTable entry)) (set! tax? #t)))))
       entries)

      ;; pre-scan invoice splits to see if any payments have been made
      (let lp ((splits splits))
        (cond
         ((null? splits) #f)
         ((equal? (xaccSplitGetParent (car splits)) txn) (lp (cdr splits)))
         (else (set! payments? #t))))
?>
  <html>
<head>
<meta charset="UTF-8"> 
<title><?scm:d (G_ "Invoice") ?> <?scm:d invoiceid ?></title>
<link rel="stylesheet" href="styles.css"/>

<style type="text/css">
<?scm:d opt-extra-css ?>
</style>
</head>
<body>

<div id="rechnungsvorlage">
  
  <header class="container">
    
    <div class="seller">
      <div class="section">
        <div><?scm:d (or coyname (G_ "Company Name")) ?></div>
        <div><?scm:d coycontact ?></div>
        <div><?scm:d (nl->br coyaddr) ?></div>
      </div>
      <div class="section">
        <div><?scm:d (G_ "Tel.") ?>: <?scm:d coyphone ?></div>
        <div><?scm:d (G_ "E-Mail") ?>: <?scm:d coyemail ?></div>
      </div>
    </div>
    
    <div class="client">
      <div class="client--seller-address">
        <?scm:d coyname ?><?scm:d delimiter ?><?scm:d (nl->delimiter coyaddr) ?>
      </div>
      <div class="client--address">
        <div><?scm:d ownername ?></div>
        <div><?scm:d (nl->br owneraddr) ?></div>
      </div>
    </div>
  </header>
  
  <article class="invoice container">   
  <section class="invoice-meta">
    <div class="floatr">
      <?scm (if (not isposted) (begin ?>
        <div>Rechnungsdatum: 'Platzhalter'</div>
      <?scm ) (begin ?>
        <div>Rechnungsdatum: <?scm:d (nbsp (gnc-print-time64 postdate dateformat)) ?></div>
    </div>
    <div class="floatl">
      <div>
        <div class="field--data">Rechnungs Nr. <?scm:d invoiceid ?></div>
        <div class="field--text">Bitte bei Zahlungen und Schriftverkehr angeben!</div>
      </div>
      <div>
        <div class="field--data one-row-top">Leistungszeitraum: <?scm:d (nbsp opt-leistungsdatum) ?></div>
        <div class="field--data one-row-top">Auftrag: <?scm:d jobnumber ?></div>
        </div>
    </div>
  </section>
  
  <section class="invoice-text"><?scm:d (nl->br notes) ?></section>
  
  <section class="invoice-table">
  <div class="table-row invoice-table--headers container">
    <div class="table-header table-col-1">Pos.</div>
    <div class="table-header table-col-2">Beschreibung</div>
    <div class="table-header table-col-3">Menge</div>
    <div class="table-header table-col-4">Einheit</div>
    <div class="table-header table-col-5">Einzelpreis</div>
    <div class="table-header table-col-6">Gesamtpreis</div>
  </div>
  <?scm
    (let* ((inv-total (gncInvoiceGetTotal opt-invoice))
           (tax-total (gncInvoiceGetTotalTax opt-invoice))
           (sub-total (gncInvoiceGetTotalSubtotal opt-invoice))
           (dsc-total (- inv-total tax-total sub-total))
           (total-col (gnc:make-commodity-collector))
           (position 1))  
      (total-col 'add currency inv-total)
      (for-each
       (lambda (entry)
         (let ((qty       (gncEntryGetDocQuantity entry credit-note?))
               (each      (gncEntryGetPrice entry cust-doc? opt-netprice))
               (rval      (gncEntryGetDocValue entry #t cust-doc? credit-note?))
               (rdiscval  (gncEntryGetDocDiscountValue entry #t cust-doc? credit-note?))
               (rtaxval   (gncEntryGetDocTaxValue entry #t cust-doc? credit-note?))
               (disc      (if cust-doc? (gncEntryGetInvDiscount entry)))
               (disctype  (gncEntryGetInvDiscountType entry))
               (countType (if units? (gncEntryGetAction entry) ""))
               (totalPrice (if opt-kleinunternehmerregelung? 
                               (fmtmoney currency rval) 
                               (fmtmoney currency (gnc-numeric-add rval rtaxval GNC-DENOM-AUTO GNC-RND-ROUND))))
               (unitPrice (fmtmoney currency each)))
         ?>
         <div class="table-row">
           <div class="table-col-1"><?scm:d position ?></div> <!-- Display the position number -->
           <div class="table-col-2"><?scm:d (gncEntryGetDescription entry) ?></div>
           <div class="table-col-3"><?scm:d (fmtnumeric qty) ?></div>
           <div class="table-col-4"><?scm:d countType ?></div>
           <div class="table-col-5"><?scm:d unitPrice ?> €</div>
           <div class="table-col-6"><?scm:d totalPrice ?> €</div>
         </div>
         <?scm
           (set! position (+ position 1))  ;; Increment position for next entry
         ))
       entries) ?>

  <div class="table-row one-row-top">
    <div class="table-col-12345">Zwischensumme</div>
    <div class="table-col-6"><?scm:d (fmtmoney currency dsc-total) ?> €</div>
  </div>
  
  <?scm (if (not opt-kleinunternehmerregelung?) (begin ?> <!-- Check Kleinunternehmer status -->
    <div class="table-row">
      <div class="table-col-12345">+19% MwSt.</div>
      <div class="table-col-6"><?scm:d (fmtmoney currency tax-total) ?> €</div>
    </div>
  <?scm )) ?>

  <div class="table-row">
    <div class="table-col-12345 bold">Rechnungsbetrag</div>
    <div class="table-col-6 bold"><?scm:d (fmtmoney currency inv-total) ?> €</div>
  </div>
</section>

  <section class="invoice-text"><?scm:d (nl->br opt-extra-notes) ?></section>
  
  </article>
    
  <footer>
    <div class="col-25">
        <div><?scm:d coycontact ?></div>
        <div><?scm:d (nl->br coyaddr) ?></div>
    </div>
    <div class="col-45">
        <div>IBAN: <?scm:d opt-bank-ibancode ?></div>
        <div>BIC: <?scm:d opt-bank-swiftcode ?></div>
        <div><?scm:d opt-bank-accountnumber ?></div>
    </div>
    <div>
        <div>USt-IdNr. PLATZHALTER</div>
    </div>
  </footer>
  
</div>

<?scm )) ; end of display-report function

  ;; 'mainline' code: check for a valid invoice, then display the report


  (cond
   ((null? opt-invoice)
    (display (string-append "<h2>" (G_ "Rechnung") "</h2>"))
    (display (string-append "<p>" (G_ "No invoice has been selected -- please use the Options menu to select one.") "</p>")))

   (else
    (display-report opt-invoice)))

?>
</body>
</html>
<?scm 
  ) ; Close the outer let
?>