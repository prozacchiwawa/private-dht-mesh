module DHTVis

open Buffer
open KBucket

let idPresentationString id =
  String.concat
    ""
    (Array.map
       (fun e -> sprintf "%02x" e)
       (Buffer.toArray id)
    )
    
let rec formatter (host : string option) (getId : 'a -> Buffer) (getHost : 'a -> string) (kb : KBucket.KBucket<Buffer,'a>) : string =
  String.concat
    ""
    ["<div class='bucket'>" ;
     (host |> optionMap (fun host -> String.concat "" ["<div class='title'>" ; string kb.ver ; " " ; host ; "</div>"]) |> optionDefault "") ;
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
                      idPresentationString (getId node) ;
                      " " ;
                      getHost node ;
                      "</div>"
                     ]
                 )
                 bucket
              ) ;
            "</div>"
           ]
      | Split (low,high) ->
         String.concat
           ""
           ["<div class='split'><div class='left'>";
            formatter None getId getHost low;
            "</div><div class='right'>";
            formatter None getId getHost high;
            "</div></div>"
           ]
     ) ;
     "</div>"
     ]

let writeFile getId getHost name kb =
  let hid = Buffer.toString "binary" (DHT.hashId name) in
  ignore
    (QIO.writeText
       (sprintf "%s.html" (idPresentationString (DHT.hashId name)))
       (String.concat
          ""
          ["<html><head><link rel='stylesheet' href='index.css'></link></head><body>" ;
           (formatter (Some name) getId getHost kb) ;
           "</body></html>"
          ]
       )
    )
