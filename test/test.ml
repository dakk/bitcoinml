open Bitcoinml;;
open OUnit2;;

let test_sign octx =
	assert_equal 0 0
;;

let suite =
	"bitcoinml.script" >::: [
		"sign" >:: test_sign ;
];;

let () = run_test_tt_main suite;;