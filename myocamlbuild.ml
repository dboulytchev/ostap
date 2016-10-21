open Ocamlbuild_plugin;;
open Command;;

dispatch begin function
 | After_rules ->

   flag ["compile";  "ocaml"; "use_ostap_lib1"] (S[A"-I"; A"src"; A"bNF3.cmo"]);
   flag ["compile";  "ocaml"; "use_ostap_lib"] (S[A"ostap.cmo"]);
   flag ["ocamldep"; "ocaml"; "use_pa_log"]
     (S [A"-pp";A"camlp5o pr_o.cmo pa_log.cmo"  ]);

   flag ["ocamldep"; "use_pa_ostap"]
     (S[A"-pp";A"camlp5o src/BNF3.cmo pr_o.cmo ./pa_ostap.cmo"]);
   flag ["compile"; "ocaml"; "use_pa_ostap"]
     (S [A"-I";A"lib";
         A"-pp";A"camlp5o src/BNF3.cmo pr_o.cmo ./pa_ostap.cmo"
	    ]);

   flag ["compile"; "byte"; "ocaml"; "use_ostap_lib"]   (S[A"ostap.cmo"]);
   flag ["compile"; "native"; "ocaml"; "use_ostap_lib"] (S[A"ostap.cmx"]);

   ()
 | _ -> ()
 end
