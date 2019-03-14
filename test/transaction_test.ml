open Bitcoinml;;
open Stdint;;
open OUnit2;;
open Hex;;

let script_parse_test sc scdec octx =
	let s = Script.parse (Hex.to_string sc) in
	assert_equal (fst s) (fst scdec)
;;

let script_serialize_test sc scdec octx =
	let s = Script.serialize scdec in
	assert_equal (Hex.to_string sc) s
;;

let script_verify_is_spendable_test sc octx =
	assert_equal (Script_verify.is_spendable sc) true
;;

let script_verify_spendable_by_test prefix sc adr octx =
	let s = Script_verify.spendable_by sc prefix in
	assert_equal s (Some adr)
;;

let tx_parse_test raw octx =
	(let raw', tx = Tx.parse (Hex.to_string raw) in
	match tx with
	| None -> assert_equal true false
	| Some (t) -> 
		assert_equal (Tx.serialize ~hex:true t) (raw |> show);
		assert_equal (Tx.serialize t) (Hex.to_string raw));
	(let raw', tx = Tx.parse ~hex:true (raw |> show) in
	match tx with
	| None -> assert_equal true false
	| Some (t) -> 
		assert_equal (Tx.serialize t) (Hex.to_string raw);
		assert_equal (Tx.serialize ~hex:true t) (raw |> show))
;;

let tx_parse_and_check_hash raw hash octx = 
	tx_parse_test raw octx;
	let raw', tx = Tx.parse (Hex.to_string raw) in
	match tx with
	| None -> assert_equal true false
	| Some (t) -> 
		assert_equal (t.hash) (hash);
		assert_equal (Tx.serialize t) (Hex.to_string raw)
;;


let tx_parse_segwit_test raw dest sizes vsize hashes ?prefix:(prefix=(Params.of_network XTN).prefixes) octx =
	tx_parse_test raw octx;
	let raw', tx = Tx.parse (Hex.to_string raw) in
	match tx with
	| None -> assert_equal true false
	| Some (t) -> 
		assert_equal (Tx.serialize t) (Hex.to_string raw);
		match Script_verify.spendable_by ((List.nth t.txout 0).script) prefix with
		| None -> assert_equal true false
		| Some (addr) -> 
			assert_equal dest addr;
			match t.Tx.witness with 
			| None -> assert_equal true false
			| Some (w) ->
				assert_equal (fst sizes) t.size;
				assert_equal (snd sizes) w.size; 
				assert_equal vsize t.vsize;
				assert_equal (snd hashes) w.hash;
				assert_equal (fst hashes) t.hash
;;


let tlist = [
	"script.parse" 			>:: script_parse_test (`Hex "76A91489ABCDEFABBAABBAABBAABBAABBAABBAABBAABBA88AC")
  ([
    Script.OP_DUP; Script.OP_HASH160; Script.OP_DATA (20, Hex.to_string (`Hex "89ABCDEFABBAABBAABBAABBAABBAABBAABBAABBA"));
    Script.OP_EQUALVERIFY; Script.OP_CHECKSIG
  ], 25);
"script.serialize" 		>:: script_serialize_test (`Hex "76A91489ABCDEFABBAABBAABBAABBAABBAABBAABBAABBA88AC")
  ([
    Script.OP_DUP; Script.OP_HASH160; Script.OP_DATA (20, Hex.to_string (`Hex "89ABCDEFABBAABBAABBAABBAABBAABBAABBAABBA"));
    Script.OP_EQUALVERIFY; Script.OP_CHECKSIG
  ], 25);
"script_verify.is_spendable"	>:: script_verify_is_spendable_test
  ([
    Script.OP_DUP; Script.OP_HASH160; Script.OP_DATA (20, Hex.to_string (`Hex "89ABCDEFABBAABBAABBAABBAABBAABBAABBAABBA"));
    Script.OP_EQUALVERIFY; Script.OP_CHECKSIG
  ], 25);
"script_verify.spendable_by"	>:: script_verify_spendable_by_test
  { pubkeyhash= 0x00; scripthash= 0x05; hrp= "bc" }
  ([
    Script.OP_DUP; Script.OP_HASH160; Script.OP_DATA (20, Hex.to_string (`Hex "89ABCDEFABBAABBAABBAABBAABBAABBAABBAABBA"));
    Script.OP_EQUALVERIFY; Script.OP_CHECKSIG
  ], 25)
  "1DYwPTpZuLjY2qApmJdHaSAuWRvEF5skCN";

"tx.parse" 		>:: tx_parse_test (`Hex "01000000017b1eabe0209b1fe794124575ef807057c77ada2138ae4fa8d6c4de0398a14f3f00000000494830450221008949f0cb400094ad2b5eb399d59d01c14d73d8fe6e96df1a7150deb388ab8935022079656090d7f6bac4c9a94e0aad311a4268e082a725f8aeae0573fb12ff866a5f01ffffffff01f0ca052a010000001976a914cbc20a7664f2f69e5355aa427045bc15e7c6c77288ac00000000");
"tx.parse2" 		>:: tx_parse_and_check_hash (`Hex "020000000001018123858b74cc156be287dbac5442508483bfe6d9a4165712e5bd0b65a9120a210100000000ffffffff0250c30000000000002200207110c7d8cf7bfa8c93d190d699571c128eac6e922ce0ca3bdf0bc3495109dffc14d606000000000016001499b9280ca4f5eeea13e51770e7a30eb512ed473702483045022100952394943abe1e75d20d4b20622416d0618585bb3266a8122894024a7cfcc6c8022051465780d56717548951fad188fe038f8820c1e054d3a9b90533084cb06625cf012102a3a3756f1e06a4b4764cc3fcf2e804257727e4431b548741f8be057f1628e39900000000") "d80c367ee53355ff52bb818545e73f806965a056d2085771e5c3c994af9f640a";
"tx.parse_big" >:: tx_parse_test (`Hex ("0100000001bbbd750cbb2929e03cf7e6dfcfddc8fc507edb7f9edcec0bcde93ad82137c767010000006b4830450221008fbeab67d9a2f5534a8bb9cae029a4e4d0431519e224a115aff9448b6dc987d902206e1123e87c2a6ec2467021326e7dd5cb356483f4d327a585738c5e7c7acdba560121027ad5e81a327971750392e620827774f7c768d1280d4e8dfe8a7b5c5ba3dcddd7ffffffff010000000000000000fe703a0f00" ^ (String.make 1996000 'f') ^ "00000000" ));
"tx.parse_witness_P2WPKH" >:: tx_parse_segwit_test 
  (`Hex "0100000000010115e180dc28a2327e687facc33f10f2a20da717e5548406f7ae8b4c811072f8560100000000ffffffff0100b4f505000000001976a9141d7cd6c75c2e86f4cbf98eaed221b30bd9a0b92888ac02483045022100df7b7e5cda14ddf91290e02ea10786e03eb11ee36ec02dd862fe9a326bbcb7fd02203f5b4496b667e6e281cc654a2da9e4f08660c620a1051337fa8965f727eb19190121038262a6c6cec93c2d3ecd6c6072efea86d02ff8e3328bbd0242b20af3425990ac00000000") 
  "miCsSagJ7RQDCMcBUaKFKEryaLnxbhGAPt"
  (85, 110) 113
  ("d869f854e1f8788bcff294cc83b280942a8c728de71eb709a2c29d10bfe21b7c", "976015741ba2fc60804dd63167326b1a1f7e94af2b66f4a0fd95b38c18ee729b");

"tx.parse_witness_P2WSH" >:: tx_parse_segwit_test 
  (`Hex "0100000000010115e180dc28a2327e687facc33f10f2a20da717e5548406f7ae8b4c811072f8560200000000ffffffff0188b3f505000000001976a9141d7cd6c75c2e86f4cbf98eaed221b30bd9a0b92888ac02483045022100f9d3fe35f5ec8ceb07d3db95adcedac446f3b19a8f3174e7e8f904b1594d5b43022074d995d89a278bd874d45d0aea835d3936140397392698b7b5bbcdef8d08f2fd012321038262a6c6cec93c2d3ecd6c6072efea86d02ff8e3328bbd0242b20af3425990acac00000000") 
  "miCsSagJ7RQDCMcBUaKFKEryaLnxbhGAPt"
  (85, 112) 113
  ("78457666f82c28aa37b74b506745a7c7684dc7842a52a457b09f09446721e11c", "12b1b3fe5e29f136bed2996d39e935442b619feb0a12d30ca832d29246689aa9");
  
"tx.parse_witness_P2SH(WPKH)" >:: tx_parse_segwit_test 
  (`Hex "0100000000010115e180dc28a2327e687facc33f10f2a20da717e5548406f7ae8b4c811072f85603000000171600141d7cd6c75c2e86f4cbf98eaed221b30bd9a0b928ffffffff019caef505000000001976a9141d7cd6c75c2e86f4cbf98eaed221b30bd9a0b92888ac02483045022100f764287d3e99b1474da9bec7f7ed236d6c81e793b20c4b5aa1f3051b9a7daa63022016a198031d5554dbb855bdbe8534776a4be6958bd8d530dc001c32b828f6f0ab0121038262a6c6cec93c2d3ecd6c6072efea86d02ff8e3328bbd0242b20af3425990ac00000000") 
  "miCsSagJ7RQDCMcBUaKFKEryaLnxbhGAPt"
  (108, 110) 136
  ("8139979112e894a14f8370438a471d23984061ff83a9eba0bc7a34433327ec21", "6bf4e4dfb860cf0906f49c836700b130ac78cc391c72a0911c94cdec4dcb10ec");

"tx.parse_witness_P2SH(WSH)" >:: tx_parse_segwit_test 
  (`Hex "0100000000010115e180dc28a2327e687facc33f10f2a20da717e5548406f7ae8b4c811072f856040000002322002001d5d92effa6ffba3efa379f9830d0f75618b13393827152d26e4309000e88b1ffffffff0188b3f505000000001976a9141d7cd6c75c2e86f4cbf98eaed221b30bd9a0b92888ac02473044022038421164c6468c63dc7bf724aa9d48d8e5abe3935564d38182addf733ad4cd81022076362326b22dd7bfaf211d5b17220723659e4fe3359740ced5762d0e497b7dcc012321038262a6c6cec93c2d3ecd6c6072efea86d02ff8e3328bbd0242b20af3425990acac00000000") 
  "miCsSagJ7RQDCMcBUaKFKEryaLnxbhGAPt"
  (120, 111) 148
  ("954f43dbb30ad8024981c07d1f5eb6c9fd461e2cf1760dd1283f052af746fc88", "a5947589e2762107ff650958ba0e3a3cf341f53281d15593530bf9762c4edab1");

"tx.parse_witness_mn" >:: tx_parse_segwit_test
  (`Hex "0200000000010140d43a99926d43eb0e619bf0b3d83b4a31f60c176beecfb9d35bf45e54d0f7420100000017160014a4b4ca48de0b3fffc15404a1acdc8dbaae226955ffffffff0100e1f5050000000017a9144a1154d50b03292b3024370901711946cb7cccc387024830450221008604ef8f6d8afa892dee0f31259b6ce02dd70c545cfcfed8148179971876c54a022076d771d6e91bed212783c9b06e0de600fab2d518fad6f15a2b191d7fbd262a3e0121039d25ab79f41f75ceaf882411fd41fa670a4c672c23ffaf0e361a969cde0692e800000000")
  "38Segwituno6sUoEkh57ycM6K7ej5gvJhM"
  (106, 110) 134
  ("c586389e5e4b3acb9d6c8be1c19ae8ab2795397633176f5a6442a261bbdefc3a", "b759d39a8596b70b3a46700b83e1edb247e17ba58df305421864fe7a9ac142ea")
  ~prefix:(Params.of_network BTC).prefixes;
];;