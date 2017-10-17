(require 's)
(require 'buttercup)
(load-file "company-graphql.el")

(describe
 "conversion between json/hash-table"
 (it "print json from hashtable"
   (let* ((json-file (expand-file-name "introspection-mock-1.json" "./tests"))
	  (mock-1  (json-read-file json-file))
	  (ans-1 "{\"data\":{\"__schema\":{\"queryType\":{\"name\":\"Query\"},\"mutationType\":{\"name\":\"Mutation\"},\"subscriptionType\":{\"name\":\"Subscription\"}}}}"))
     (expect (json-encode mock-1) :to-equal ans-1)
     )
   )
 )
