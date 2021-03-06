#lang racket/base
(require
 racket/string
 (planet bzlib/date-tz/plt)
 "util.rkt")

(provide <timezone>
         <timezone-abbrev>)

; Timezone names
; http://en.wikipedia.org/wiki/List_of_time_zone_abbreviations
(define timezones
  '((ACDT +1030)
    (ACST +0930)
    (ACT +0800)
    (ADT -0300)
    (AEDT +1100)
    (AEST +1000)
    (AFT +0430)
    (AKDT -0800)
    (AKST -0900)
    (AMST +0500)
    (AMT +0400)
    (ART -0300)
    (AST -0400)
    (AWDT +0900)
    (AWST +0800)
    (AZOST -0100)
    (AZT +0400)
    (BDT +0800)
    (BIOT +0600)
    (BIT -1200)
    (BOT -0400)
    (BRT -0300)
    (BST +0100) ; British Summer Time, not Bangladesh Standard Time
    (BTT +0600)
    (CAT +0200)
    (CCT +0630)
    (CDT -0500)
    (CEDT +0200)
    (CEST +0200)
    (CET +0100)
    (CHADT +1345)
    (CHAST +1245)
    (CIST -0800)
    (CKT -1000)
    (CLST -0300)
    (CLT -0400)
    (COST -0400)
    (COT -0500)
    (CST -0600) ; Central Standard Time (North America), not China Standard Time or Central Standard Time (Australia)
    (CT +0800)
    (CVT -0100)
    (CXT +0700)
    (CHST +1000)
    (DFT +0100)
    (EAST -0600)
    (EAT +0300)
    (ECT -0500) ; Ecuador Time, not Eastern Caribbean Time
    (EDT -0400)
    (EEDT +0300)
    (EEST +0300)
    (EET +0200)
    (EST -0500)
    (FJT +1200)
    (FKST -0300)
    (FKT -0400)
    (GALT -0600)
    (GET +0400)
    (GFT -0300)
    (GILT +1200)
    (GIT -0900)
    (GMT -0000)
    (GST +0400) ; Gulf Standard Time, not South Georgia and South Sandwich Islands
    (GYT -0400)
    (HADT -0900)
    (HAEC +0200)
    (HAST -1000)
    (HKT +0800)
    (HMT +0500)
    (HST -1000)
    (ICT +0700)
    (IDT +0300)
    (IRKT +0800)
    (IRST +0330)
    (IST +0530) ; Indian Standard Time, not Irish Summer Time or Israel Standard Time
    (JST +0900)
    (KRAT +0700)
    (KST +0900)
    (LHST +1030)
    (LINT +1400)
    (MAGT +1100)
    (MDT -0600)
    (MET +0200)
    (MEST +0200)
    (MIT -0930)
    (MSD +0400)
    (MSK +0300)
    (MST -0700) ; Mountain Standard Time (North America), not Malaysian Standard Time
    (MUT +0400)
    (MYT +0800)
    (NDT -0230)
    (NFT +1130)
    (NPT +0545)
    (NST -0330)
    (NT -0330)
    (NZDT +1300)
    (NZST +1200)
    (OMST +0600)
    (PDT -0700)
    (PETT +1200)
    (PHOT +1300)
    (PKT +0500)
    (PST -0800) ; Pacific Standard Time (North America), not Phillipine Standard Time
    (RET +0400)
    (SAMT +0400)
    (SAST +0200)
    (SBT +1100)
    (SCT +0400)
    (SGT +0800)
    (SLT +0530)
    (SST -1100) ; Samoa Standard Time, not Singapore Standard Time
    (TAHT -1000)
    (THA +0700)
    (UTC -0000)
    (UYST -0200)
    (UYT -0300)
    (VET -0430)
    (VLAT +1000)
    (WAT +0100)
    (WEDT +0100)
    (WEST +0100)
    (WET -0000)
    (WST +0800)
    (YAKT +0900)
    (YEKT +0500)))

(define <timezone-abbrev>
  (pair-list->parser
   (sort (map (lambda (tz)
                (cons (string-downcase (symbol->string (car tz))) tz))
              timezones)
         (lambda (a b)
           (> (string-length (car a))
              (string-length (car b)))))))

(define timezone-names
  (map (lambda (tz)
         (list
          (string-replace (string-downcase tz) "_" " ") tz))
       (tz-names)))

(define <timezone>
  (pair-list->parser
   (sort timezone-names
         (lambda (a b)
           (> (string-length (car a))
              (string-length (car b)))))))
