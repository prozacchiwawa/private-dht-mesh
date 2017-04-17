module DHTVis

let idPresentationString id =
  String.concat
    ""
    (Array.map
       (fun e -> sprintf "%02x" e)
       (Buffer.toArray id)
    )
    
let rec formatter kb : string =
  String.concat
    ""
    ["<div class='bucket'>" ;
     (match kb.storage with
      | Self bucket ->
         String.concat
           ""
           ["<div class='content'>" ;
            String.concat
              ""
              (Array.map
                 (fun node ->
                   String.concat
                     ""
                     ["<div class='id'>" ;
                      idPresentationString node.id ;
                      "</div>"
                     ]
                 )
                 bucket
              ) ;
            "</div>"
           ]
      | Split (low,high) ->
         String.concat "" ["<div class='split'><div class='left'>";formatter low;"</div><div class='right'>";formatter high;"</div></div>"]
     ) ;
     "</div>"
    ]

let writeFile i allDhts =
  let hid = Buffer.toString "binary" (DHT.hashId (string i)) in
  ignore
    (QIO.writeText
       (sprintf "%s.html" (idPresentationString (DHT.hashId (string i))))
       (String.concat
          ""
          ["<html><head><link rel='stylesheet' href='index.css'></link></head><body>" ;
           (formatter (Map.find hid allDhts.dhts).nodes) ;
           "</body></html>"
          ]
       )
    )

let writeFiles n allDhts =
  for i = 0 to n do
    begin
      let _ = printfn "Writing %d" in
      writeFile i
    end
      
