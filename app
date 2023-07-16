# Load the required packages
library(shiny)
library(DT)
library(readxl)
library(shinyWidgets)
library(shinythemes)
library(plotly) 
library(lubridate)


# Load the credit card database
credit_card_db <- structure(list(Rank = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 
                                          13, 14, 15, 15, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 
                                          27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 
                                          43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 
                                          59, 60, 61, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 
                                          74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, NA
), Bank = c("U.S. Bank", "Chase", "Capital One", "American Express", 
            "American Express", "American Express", "American Express", "Barclays", 
            "Bank of America", "Barclays", "American Express", "American Express", 
            "Bank of America", "Bank of America", "Chase", "Chase", "Chase", 
            "Bank of America", "Barclays", "Lafayette Federal Credit Union", 
            "Chase", "Citi", "Barclays", "PenFed", "Fremont Bank", "Barclays", 
            "Best Western Rewards", "Wescom Credit Union", "American Express", 
            "American Express", "Wells Fargo", "Citi", "NASA Federal", "Capital One", 
            "Discover", "Ridgewood Savings Bank", "American Express", "American Express", 
            "Wells Fargo", "Citi", "American Express", "Bank of America", 
            "FNBO", "Discover", "Chase", "Chase", "Citi", "Chase", "Gesa", 
            "U.S. Bank", "Barclays", "Amazon", "Fidelity", "Paypal", "Citi", 
            "Sallie Mae", "SoFi", "Citizens Bank", "Citi", "Apple", "American Express", 
            "Chase", "Chase", "Chase", "American Express", "U.S. Bank", "American Express", 
            "Chase", "Bank of America", "Bank of America", "PNC", "Zions Bank", 
            "Capital on Tap", "American Express", "Bank of America", "Chase", 
            "U.S. Bank", "Ridgewood Savings Bank", "American Express", "Union Bank", 
            "Huntington", "1st Source Bank", "FNBO", "American Express", 
            "American Express", "American Express", "American Express", "American Express", 
            "American Express", "American Express", "American Express", NA
), `Card Name` = c("U.S. Bank Altitude Reserve", "Chase  Sapphire Preferred", 
                   "Capital One Venture", "American Express Platinum", "American Express Platinum", 
                   "American Express Green", "American Express Hilton Surpass", 
                   "Barclays Aviator Red Red World Elite MasterCard", "Bank of America Alaskan Airlines Card", 
                   "Barclays Lufthansa World Elite Mastercard", "American Express Gold", 
                   "American Express  Gold", "Bank of America Premium Rewards Credit Card", 
                   "Bank of America Premium Rewards Elite Card", "Chase Southwest Plus", 
                   "Chase Southwest Premier", "Chase Southwest Priority", "Bank of America Air France/KLM FlyingBlue Credit Card", 
                   "Barclays  Wyndham Cards", "Lafayette Federal Credit Union Mastercard", 
                   "Chase  United Explorer", "Citi  Premier", "Barclays Hawaiian Airlines", 
                   "PenFed  Pathfinder Rewards Card", "Fremont Bank Mastercard", 
                   "Barclays Emirates Cards", "Best Western Rewards Premium Mastercard", 
                   "Wescom Credit Union Bruin Edge Visa", "American Express Hilton Honors Card", 
                   "American Express Cash Magnet Card", "Wells Fargo Active Cash Credit Card", 
                   "Citi Sears Shop Your Way Mastercard", "NASA Federal Star Trek Credit Card", 
                   "Capital One SavorOne Card", "Discover  Cash Back Credit Card", 
                   "Ridgewood Savings Bank Mastercard", "American Express Delta Gold", 
                   "American Express Blue Cash Preferred", "Wells Fargo Autograph Card", 
                   "Citi  Rewards Mastercard", "American Express Blue Cash Everyday", 
                   "Bank of America Unlimited Cash Rewards Card", "FNBO  FordPass Rewards Visa Card", 
                   "Discover  Cash Back Credit Card", "Chase  Chase Freedom Credit Cards", 
                   "Chase  Instacart Mastercard", "Citi  Custom Cash Card", "Chase  Disney Premier Credit Card", 
                   "Gesa  Cash Rewards Card", "U.S. Bank Visa Card", "Barclays  Upromise Mastercard", 
                   "Amazon Amazon Store Card", "Fidelity  Visa CashBack Card", "Paypal  Cashback Mastercard", 
                   "Citi AT&T Points Plus Card", "Sallie Mae Accelerate", "SoFi Credit Card", 
                   "Citizens Bank Mastercard", "Citi  AT&T Points Plus Card", "Apple  Apple Card", 
                   "American Express Business Platinum", "Chase  Ink Preferred", 
                   "Chase  Ink Cash", "Chase  Ink Unlimited", "American Express Business Gold", 
                   "U.S. Bank Leverage Business Card", "American Express Business Delta Gold", 
                   "Chase  Southwest Business Performance", "Bank of America Business Cash Rewards", 
                   "Bank of America Launches Business Advantage Unlimited Cash Rewards", 
                   "PNC  BusinessOptions Credit Card", "Zions Bank AmaZing Business Credit Card", 
                   "Capital on Tap Business Credit Card", "American Express Hilton Honors Business Card", 
                   "Bank of America Business Advantage Travel Rewards", "Chase IHG Premier Business Card", 
                   "U.S. Bank  Triple Cash Rewards Business Card", "Ridgewood Savings Bank Business Credit Cards", 
                   "American Express Blue Business Cash Card", "Union Bank Business Rewards Visa Card", 
                   "Huntington  Business Voice Credit Card", "1st Source Bank Business Rewards Card", 
                   "FNBO  Evergreen Business Card", "American Express Business Platinum", 
                   "American Express Business Gold", "American Express Blue Business Cash Card", 
                   "American Express Blue Business Plus", "American Express AmEx EveryDay Credit Card", 
                   "American Express Hilton Surpass", "American Express Express Delta", 
                   "American Express Business Gold", NA), `Business or Personal` = c("Personal", 
                                                                                     "Personal", "Personal", "Personal", "Personal", "Personal", "Personal", 
                                                                                     "Personal", "Personal", "Personal", "Personal", "Personal", "Personal", 
                                                                                     "Personal", "Personal", "Personal", "Personal", "Personal", "Personal", 
                                                                                     "Personal", "Personal", "Personal", "Personal", "Personal", "Personal", 
                                                                                     "Personal", "Personal", "Personal", "Personal", "Personal", "Personal", 
                                                                                     "Personal", "Personal", "Personal", "Personal", "Personal", "Personal", 
                                                                                     "Personal", "Personal", "Personal", "Personal", "Personal", "Personal", 
                                                                                     "Personal", "Personal", "Personal", "Personal", "Personal", "Personal", 
                                                                                     "Personal", "Personal", "Personal", "Personal", "Personal", "Personal", 
                                                                                     "Personal", "Personal", "Personal", "Personal", "Personal", "Business", 
                                                                                     "Business", "Business", "Business", "Business", "Business", "Business", 
                                                                                     "Business", "Business", "Business", "Business", "Business", "Business", 
                                                                                     "Business", "Business", "Business", "Business", "Business", "Business", 
                                                                                     "Business", "Business", "Business", "Business", "YMMV/Targeted Offers", 
                                                                                     "YMMV/Targeted Offers", "YMMV/Targeted Offers", "YMMV/Targeted Offers", 
                                                                                     "YMMV/Targeted Offers", "YMMV/Targeted Offers", "YMMV/Targeted Offers", 
                                                                                     "YMMV/Targeted Offers", NA), `Signup Bonus` = c(50000, 70000, 
                                                                                                                                     75000, 150000, 1e+05, 60000, 130000, 70000, 70000, 50000, 75000, 
                                                                                                                                     90000, 60000, 75000, 60000, 60000, 60000, 70000, 75000, 50000, 
                                                                                                                                     70000, 75000, 70000, 50000, 50000, 60000, 80000, 50000, 80000, 
                                                                                                                                     20000, 20000, 22500, 30000, 30000, 10000, 20000, 50000, 30000, 
                                                                                                                                     30000, 25000, 25000, 20000, 10000, 10000, 20000, 30000, 20000, 
                                                                                                                                     30000, 10000, 20000, 20000, 10000, 15000, 10000, 10000, 20000, 
                                                                                                                                     20000, 20000, 10000, 7500, 150000, 1e+05, 90000, 90000, 85000, 
                                                                                                                                     75000, 70000, 70000, 50000, 30000, 75000, 50000, 75000, 150000, 
                                                                                                                                     50000, 165000, 50000, 50000, 25000, 50000, 15000, 20000, 20000, 
                                                                                                                                     250000, 180000, 75000, 75000, 25000, 150000, 50000, 35000, NA
                                                                                     ), `Sign-up Bonus Value` = c(750, 1400, 1387.5, 3000, 2000, 1200, 
                                                                                                                  780, 1239, 1260, 500, 1500, 1800, 600, 750, 900, 900, 900, 700, 
                                                                                                                  825, 500, 770, 1350, 630, 500, 500, 720, 800, 500, 480, 200, 
                                                                                                                  200, 225, 300, 555, 100, 200, 705, 600, 300, 450, 500, 200, 100, 
                                                                                                                  100, 400, 300, 200, 300, 100, 400, 200, 100, 150, 100, 100, 200, 
                                                                                                                  200, 200, 100, 75, 3000, 2000, 1800, 1800, 1700, 750, 987, 1050, 
                                                                                                                  500, 300, 750, 500, 750, 900, 500, 825, 500, 500, 500, 500, 150, 
                                                                                                                  200, 200, 5000, 3600, 1500, 1500, 500, 900, 705, 700, NA), `Spend Requirement` = c(4500, 
                                                                                                                                                                                                     6000, 4000, 6000, 6000, 3000, 2000, 0, 3000, 3000, 4000, 4000, 
                                                                                                                                                                                                     4000, 5000, 3000, 3000, 3000, 2000, 1000, 5000, 4000, 4000, 2000, 
                                                                                                                                                                                                     3000, 5000, 3000, 3000, 2500, 1000, 2000, 1000, 1500, 2000, 3000, 
                                                                                                                                                                                                     500, 2000, 2000, 3000, 1500, 1500, 2000, 1000, 3000, 100, 500, 
                                                                                                                                                                                                     0, 1500, 1000, 1000, 1000, 500, 0, 1500, 500, 1000, 1000, 1500, 
                                                                                                                                                                                                     500, 1000, 0, 15000, 15000, 6000, 6000, 10000, 7500, 2000, 5000, 
                                                                                                                                                                                                     5000, 3000, 25000, 3000, 7500, 4000, 5000, 3000, 4500, 5000, 
                                                                                                                                                                                                     5000, 5000, 2000, 500, 3000, 30000, 20000, 15000, 15000, 2000, 
                                                                                                                                                                                                     2000, 2000, 5000, NA), `Spend Timeline (days)` = c(90, 180, 90, 
                                                                                                                                                                                                                                                        180, 180, 180, 90, 90, 90, 90, 180, 180, 90, 90, 90, 90, 90, 
                                                                                                                                                                                                                                                        90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 180, 90, 90, 
                                                                                                                                                                                                                                                        90, 90, 90, 90, 90, 180, 90, 90, 180, 90, 90, 90, 90, 0, 180, 
                                                                                                                                                                                                                                                        90, 90, 120, 90, 0, 90, 90, 90, 90, 0, 90, 90, 30, 90, 90, 90, 
                                                                                                                                                                                                                                                        90, 90, 120, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 150, 90, 
                                                                                                                                                                                                                                                        180, 90, 60, 90, 90, 180, 180, 365, 365, 90, 90, 90, 90, NA), 
`Annual Fee` = c(400, 95, 95, 695, 695, 150, 95, 99, 95, 
                 89, 250, 250, 95, 550, 69, 69, 69, 89, 75, 0, 250, 95, 99, 
                 95, 95, 499, 89, 0, 0, 0, 0, 0, 39, 95, 0, 95, 99, 95, 0, 
                 0, 0, 0, 0, 0, 0, 0, 0, 49, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                 0, 0, 595, 95, 0, 0, 295, 95, 0, 199, 0, 0, 500, 0, 0, 95, 
                 0, 99, 0, 95, 0, 0, 0, 95, 0, 695, 295, 0, 0, 0, 95, 95, 
                 295, NA), Link = c("https://www.usbank.com/credit-cards/altitude-reserve-visa-infinite-credit-card.html", 
                                    "https://secure04ea.chase.com/web/oao/application/card?sourceCode=H57C&action=guest#/origination/cardDetails/index/initiateConFullApp;cellCode=6TKW;cfgCode=FULLAPPCONCC;channel=C30;sourceCode=H57C;AOC=6530;RPC=0444;combo=Y;params=,,,no,no,,,", 
                                    "https://applynow.capitalone.com/?productId=23312", "https://www.americanexpress.com/us/credit-cards/card-application/apply/platinum-card/46100-10-0?referring_domain=https%3A%2F%2Fwww.doctorofcredit.com%2F&exp=B#/", 
                                    "https://www.americanexpress.com/us/credit-cards/card-application/apply/platinum-card/46100-10-0?referring_domain=https%3A%2F%2Fwww.doctorofcredit.com%2F&exp=B#/", 
                                    "https://www.americanexpress.com/us/credit-cards/card-application/apply/american-express-green-card/25330-10-0?pmccode=145&intlink=US-Acq-Shop-Consumer-PDP-Green-Prospect-Apply-siderail", 
                                    "https://www.americanexpress.com/us/credit-cards/card-application/apply/partner/personal-card/hil/hilton-honors-surpass-credit-card/ep-hscc-7064#/", 
                                    "https://cards.barclaycardus.com/banking/cards/aadvantage-aviator-red-world-elite-mastercard/", 
                                    "https://secure.bankofamerica.com/apply-credit-cards/public/consumer/#/system-down/", 
                                    "https://cards.barclaycardus.com/banking/cards/lufthansa-miles-more-world-elite-mastercard/", 
                                    "https://resy.com/amex-offers?date=2023-07-09&seats=2", "https://resy.com/amex-offers?date=2023-07-09&seats=2", 
                                    "https://www.bankofamerica.com/credit-cards/products/premium-rewards-credit-card/?campaign=4065108~3G~en_US", 
                                    "https://www.bankofamerica.com/credit-cards/products/premium-rewards-elite-credit-card/", 
                                    "https://creditcards.chase.com/a1/southwest/AEP60kpromoPlus523#", 
                                    "https://creditcards.chase.com/a1/southwest/AEP60kpromoPlus523#", 
                                    "https://creditcards.chase.com/a1/southwest/AEP60kpromoPlus523#", 
                                    "https://www.flyingblue.us/en/financial/overview/financial-services-bank-of-america", 
                                    "https://cards.barclaycardus.com/banking/cards/wyndham-rewards-earner-plus-card/", 
                                    "https://www.lfcu.org/personal/credit-cards/", "https://www.theexplorercard.com/united-rewards-cards", 
                                    "https://www.citi.com/credit-cards/citi-premier-credit-card", 
                                    "https://cards.barclaycardus.com/banking/offer-expired/", 
                                    "https://www.penfed.org/credit-cards/pathfinder-rewards", 
                                    "https://www.fremontbank.com/personal/credit/world-mastercard", 
                                    "https://www.emirates.com/us/english/destinations_offers/barclays-partnership/", 
                                    "https://www.card.fnbo.com/landing/bestwestern/premium", 
                                    "https://www.wescom.org/Credit-Cards", "https://apply.americanexpress.com/en-us/hilton-cobrand-lto-april23/page/97", 
                                    "https://www.americanexpress.com/us/credit-cards/card/cash-magnet/", 
                                    "https://creditcards.wellsfargo.com/active-cash-credit-card/?sub_channel=REF&vendor_code=LH", 
                                    "https://www.shopyourway.com/credithub/overview", "https://www.nasafcu.com/personal/credit-cards/credit-card-features/visa-platinum-advantage-rewards", 
                                    "https://www.capitalone.com/credit-cards/savor-dining-rewards/", 
                                    "https://www.discover.com/credit-cards/cash-back/it-card.html", 
                                    "https://www.ridgewoodbank.com/home/personal/cardpromotion", 
                                    "https://www.americanexpress.com/us/credit-cards/card/delta-skymiles-gold-american-express-card/", 
                                    "https://www.americanexpress.com/us/credit-cards/card/blue-cash-preferred/?eep=25330&linknav=US-Acq-Shop-Consumer-VAC-Prospect-ViewCardDetail-BCP&intlink=US-Acq-Shop-Consumer-VAC-BrowseAll-Prospect-ViewCardDetail-BCP", 
                                    "https://creditcards.wellsfargo.com/autograph-visa-credit-card/?sub_channel=WEB&vendor_code=WF", 
                                    "https://www.citi.com/credit-cards/citi-rewards-plus-credit-card", 
                                    "https://www.americanexpress.com/us/credit-cards/card/blue-cash-everyday/", 
                                    "https://www.bankofamerica.com/credit-cards/products/unlimited-cash-back-credit-card/", 
                                    "https://www.ford.com/support/fordpass/fordpass-rewards/credit-card/", 
                                    "https://www.discovercard.com/application/website/apply?srcCde=GJVJ&cmpgnid=raf-dca-consumer-it&scmpgnid=7231643922753012224_&iq_id=yraf_1034755729_ps_15_234270886552&extole_shareable_code=ENI07&source=RAF", 
                                    "https://creditcards.chase.com/cash-back-credit-cards/freedom?iCELL=61FY", 
                                    "https://creditcards.chase.com/a1/instacart/100expressFA?", 
                                    "https://citicards.citi.com/usc/LPACA/Citi/Cards/CustomCash/ATL/index.html&ProspectID=PGJbUsK6GcFwRAFVMesabgVYMtqVQ6A2", 
                                    "https://creditcards.chase.com/rewards-credit-cards/disney/premier", 
                                    "https://www.gesa.com/credit-card-browse/", "https://www.usbank.com/credit-cards/cash-plus-visa-signature-credit-card.html", 
                                    "https://cards.barclaycardus.com/banking/cards/upromise-world-mastercard/", 
                                    "https://www.amazon.com/Synchrony-Bank-Amazon-com-Store-Card/dp/B008A0GNA8", 
                                    "https://www.fidelity.com/go/visa-signature-rewards-1502", 
                                    "https://www.paypal.com/us/digital-wallet/manage-money/paypal-cashback-mastercard", 
                                    "https://www.att.com/deals/att-points-plus-citi/", "https://www.salliemae.com/?dtd_cell=", 
                                    "https://partners.sofi.com/creditcard-mg-1/", "https://www.citizensbank.com/credit-cards/cash-back-world-Mastercard.aspx", 
                                    "https://www.att.com/deals/att-points-plus-citi/", "https://apply.applecard.apple/?cid=apy-616-10000012&utm_content=referdailycash", 
                                    "https://www.americanexpress.com/us/credit-cards/business/business-credit-cards/?intlink=us-en-hp-product1-business-businesscards", 
                                    "https://creditcards.chase.com/business-credit-cards/ink/business-preferred", 
                                    "https://creditcards.chase.com/business-credit-cards/ink/unlimited", 
                                    "https://creditcards.chase.com/business-credit-cards/ink/unlimited", 
                                    "https://www.americanexpress.com/us/credit-cards/business/business-credit-cards/american-express-business-gold-card-amex/62550/?linknav=US-Acq-GCP-BusinessCards-ViewAllCards-CardArt-Gold", 
                                    "https://www.usbank.com/business-banking/business-credit-cards/business-leverage-rewards-credit-card.html", 
                                    "https://www.americanexpress.com/en-us/business/credit-cards/delta-skymiles-gold/", 
                                    "https://creditcards.chase.com/business-credit-cards/southwest/performance-business", 
                                    "https://secure.bankofamerica.com/applynow/welcome.go", "https://www.bankofamerica.com/smallbusiness/credit-cards/products/unlimited-cash-rewards-business-credit-card/", 
                                    "https://www.pnc.com/en/small-business/borrowing/business-credit-cards/pnc-businessoptions-visa-signature-credit-card.html", 
                                    "https://www.zionsbank.com/business-banking/business-credit-cards/", 
                                    "https://www.doctorofcredit.com/capital-on-tap-business-credit-card-199-sign-up-bonus-2-cashback-on-all-purchases/", 
                                    "https://apply.americanexpress.com/en-us/hilton-cobrand-lto-april23/page/92", 
                                    "https://promotions.bankofamerica.com/smallbusiness/smallbusinesscards?", 
                                    "https://www.ihg.com/onerewards/content/us/en/creditcard/business", 
                                    "https://www.usbank.com/business-banking/business-credit-cards/business-triple-cash-back-credit-card.html", 
                                    "https://www.ridgewoodbank.com/home/business/cardpromotion", 
                                    "https://www.americanexpress.com/us/credit-cards/business/business-credit-cards/compare-credit-cards/compare-cash-back-business-credit-cards/", 
                                    "https://www.usbank.com/business-banking/business-credit-cards.html?redirectedFrom=UB", 
                                    "https://www.huntington.com/SmallBusiness/voice-business-credit-card", 
                                    "https://creditcardlearnmore.com/11t3/smart-business-rewards?ecdma-lc=09372&ecid=OTHE_25940", 
                                    "https://www.fnbo.com/small-business/credit-cards/evergreen", 
                                    "https://www.americanexpress.com/us/credit-cards/card-application/apply/business-platinum-charge-card/offer-no-longer-available?messageId=ec53ce1cd91541edbf349be5f0f34cc6#/", 
                                    "https://www.americanexpress.com/us/credit-cards/business/business-credit-cards/?linknav=US-Acq-GCP-Open-CardDetail-GoldCard-SubNav-BusinessesCards-ViewAllBusinessCards", 
                                    "https://www.americanexpress.com/en-us/campaigns/small-business/credit-cards/blue-business-cash/limited-time-offer/", 
                                    "https://www.americanexpress.com/en-us/campaigns/small-business/credit-cards/blue-business-plus/limited-time-offer/index1.html", 
                                    "https://www.americanexpress.com/us/credit-cards/card/amex-everyday/", 
                                    "https://www.americanexpress.com/us/credit-cards/card/hilton-honors-surpass/", 
                                    "https://www.deltaamexcard.com/", "https://www.americanexpress.com/us/credit-cards/business/business-credit-cards/american-express-business-gold-card-amex/", 
                                    NA), `Dollars per Point` = c(0.015, 0.02, 0.0185, 0.02, 0.02, 
                                                                 0.02, 0.006, 0.0177, 0.018, 0.01, 0.02, 0.02, 0.01, 0.01, 
                                                                 0.015, 0.015, 0.015, 0.01, 0.011, 0.01, 0.011, 0.018, 0.009, 
                                                                 0.01, 0.01, 0.012, 0.01, 0.01, 0.006, 0.01, 0.01, 0.01, 0.01, 
                                                                 0.0185, 0.01, 0.01, 0.0141, 0.02, 0.01, 0.018, 0.02, 0.01, 
                                                                 0.01, 0.01, 0.02, 0.01, 0.01, 0.01, 0.01, 0.02, 0.01, 0.01, 
                                                                 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.02, 0.02, 
                                                                 0.02, 0.02, 0.02, 0.01, 0.0141, 0.015, 0.01, 0.01, 0.01, 
                                                                 0.01, 0.01, 0.006, 0.01, 0.005, 0.01, 0.01, 0.02, 0.01, 0.01, 
                                                                 0.01, 0.01, 0.02, 0.02, 0.02, 0.02, 0.02, 0.006, 0.0141, 
                                                                 0.02, NA)), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, 
                                                                                                                                     -92L))

# Define UI for application
ui <- shinytheme("cerulean") %>% fluidPage(  # Apply the cerulean theme
  titlePanel("Credit Card Signup Bonus App"),
  fluidRow(
    column(4,  # Set the width of the first column to 4/12 (or 1/3) of the screen width
           shinyWidgets::pickerInput(  # Replace checkboxGroupInput with pickerInput
             inputId = "cards", 
             label = "Select the cards you already have:", 
             choices = credit_card_db$`Card Name`, 
             options = list(`actions-box` = TRUE), 
             multiple = TRUE
           ),
           numericInput("spending", "Enter your monthly spending:", value = 5000),
           textOutput("next_year_points"),  # Moved to first column
           textOutput("next_card"),  # Moved to first column
           textOutput("points")  # Moved to first column
    ),
    column(8,  # Set the width of the second column to 8/12 (or 2/3) of the screen width
           plotlyOutput("points_plot")  # Add a Plotly output for the points balance plot
    )
  ),
  DT::dataTableOutput("next_year_cards")  # Place the table output outside the fluidRow
)



# Define server logic
server <- function(input, output) {
  # Calculate recommended next card
  output$next_card <- renderText({
    recommended_cards <- recommended_cards()
    next_card <- recommended_cards[1,]$`Card Name`
    return(paste("The next card you should get is:", next_card))
  })
  
  
  # Calculate accumulated points
  output$points <- renderText({
    current_cards <- input$cards
    points <- sum(credit_card_db[credit_card_db$`Card Name` %in% current_cards, ]$`Signup Bonus`)
    return(paste("You have accumulated", points, "points."))
  })
  
  # Calculate points for next year
  output$next_year_points <- renderText({
    recommended_cards <- recommended_cards()
    total_points <- formatC(sum(as.numeric(gsub(",", "", recommended_cards$`Signup Bonus`))), format = "f", digits = 0, big.mark = ",")
    signup_value <- paste0("$", formatC(sum(as.numeric(gsub("\\$", "", gsub(",", "", recommended_cards$`Sign-up Bonus Value`)))), format = "f", digits = 0, big.mark = ","))
    paste("Over the next year, you will have accumulated", total_points, "points, worth a total of", signup_value)
  })
  
  # Calculate recommended cards for next year
  recommended_cards <- reactive({
    current_cards <- input$cards
    monthly_spending <- input$spending
    yearly_spending <- monthly_spending * 12
    
    available_cards <- credit_card_db[!credit_card_db$`Card Name` %in% current_cards, ]
    available_cards <- available_cards[order(available_cards$Rank), ]
    
    recommended_cards <- data.frame()
    total_spending <- 0
    total_months <- 0
    current_month_spending <- 0
    
    for(i in 1:nrow(available_cards)) {
      spend_requirement <- available_cards[i, ]$`Spend Requirement`
      spend_timeline <- available_cards[i, ]$`Spend Timeline (days)` / 30
      actual_spend_timeline <- ceiling(spend_requirement / monthly_spending)
      if (!is.na(spend_requirement) && !is.na(spend_timeline) &&
          (total_spending + spend_requirement <= yearly_spending) &&
          (total_months + actual_spend_timeline <= 12) &&
          (current_month_spending + spend_requirement <= monthly_spending)) {
        recommended_cards <- rbind(recommended_cards, available_cards[i, ])
        total_spending <- total_spending + spend_requirement
        total_months <- total_months + actual_spend_timeline
        current_month_spending <- current_month_spending + spend_requirement
      } else {
        current_month_spending <- 0
      }
    }
    
    # Remove columns 1 and 4
    recommended_cards <- recommended_cards[, !(names(recommended_cards) %in% c("1", "4", "Dollars per Point", "Bank"))]
    recommended_cards
  })
  
  # Render points balance plot
  output$points_plot <- renderPlotly({
    recommended_cards <- recommended_cards()[1:12,]  # Limit to first 12 rows
    recommended_cards$Month <- seq_along(recommended_cards$`Card Name`)  # Add a Month column
    recommended_cards$CumulativeValue <- cumsum(as.numeric(gsub("\\$", "", gsub(",", "", recommended_cards$`Sign-up Bonus Value`))))  # Calculate cumulative bonus value
    
    # Generate month names
    month_names <- month.name[((month(Sys.Date()) + 0:11) %% 12) + 1]  # Get next 12 month names starting from current month
    year_names <- year(Sys.Date()) + (month(Sys.Date()) + 0:11) %/% 12  # Get the corresponding years
    x_labels <- paste(month_names, year_names)  # Combine month and year into labels
    
    plot_ly(recommended_cards, x = ~Month, y = ~CumulativeValue, type = 'scatter', mode = 'lines') %>%  # Remove markers from lines
      layout(title = "Sign-up Bonus Value Over Time",
             xaxis = list(title = "Month", tickvals = ~Month, ticktext = x_labels, showgrid = FALSE),  # Use custom month labels and remove grid lines
             yaxis = list(title = "Value ($)", tickformat = "$,.0f", showgrid = FALSE)) %>%  # Format y-axis as currency and remove grid lines
      config(displayModeBar = FALSE)  # Remove the toolbar
  })
  wy
  
  
  
  output$next_year_cards <- DT::renderDataTable({
    recommended_cards <- recommended_cards()
    
    # Format the table
    recommended_cards$`Signup Bonus` <- formatC(recommended_cards$`Signup Bonus`, format = "f", digits = 0, big.mark = ",")
    recommended_cards$`Spend Requirement` <- paste0("$", formatC(recommended_cards$`Spend Requirement`, format = "f", digits = 0, big.mark = ","))
    recommended_cards$`Spend Timeline (days)` <- formatC(recommended_cards$`Spend Timeline (days)`, format = "f", digits = 0, big.mark = ",")
    recommended_cards$`Annual Fee` <- paste0("$", formatC(recommended_cards$`Annual Fee`, format = "f", digits = 0, big.mark = ","))
    recommended_cards$`Sign-up Bonus Value` <- paste0("$", formatC(recommended_cards$`Sign-up Bonus Value`, format = "f", digits = 0, big.mark = ","))
    
    # Add hyperlinks if 'Application Link' column exists
    if ("Application Link" %in% colnames(recommended_cards)) {
      recommended_cards$`Application Link` <- paste0("<a href='", recommended_cards$`Application Link`, "' target='_blank'>Go to site</a>")
    }
    
    # Add a totals row
    totals <- rep("", ncol(recommended_cards))
    names(totals) <- names(recommended_cards)
    totals["Card Name"] <- "Total"
    totals["Signup Bonus"] <- formatC(sum(as.numeric(gsub(",", "", recommended_cards$`Signup Bonus`))), format = "f", digits = 0, big.mark = ",")
    totals["Spend Requirement"] <- paste0("$", formatC(sum(as.numeric(gsub("\\$", "", gsub(",", "", recommended_cards$`Spend Requirement`)))), format = "f", digits = 0, big.mark = ","))
    totals["Annual Fee"] <- paste0("$", formatC(sum(as.numeric(gsub("\\$", "", gsub(",", "", recommended_cards$`Annual Fee`)))), format = "f", digits = 0, big.mark = ","))
    totals["Sign-up Bonus Value"] <- paste0("$", formatC(sum(as.numeric(gsub("\\$", "", gsub(",", "", recommended_cards$`Sign-up Bonus Value`)))), format = "f", digits = 0, big.mark = ","))
    
    recommended_cards <- rbind(recommended_cards, totals)
    
    datatable(recommended_cards, options = list(autoWidth = TRUE, lengthChange = FALSE, searching = FALSE, pageLength = -1), rownames = FALSE, escape = FALSE)  # Disable text escaping to allow HTML
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
