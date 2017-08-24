open Stdint;;

type e = BTC | XTN | BCH | SIDECHAIN | NOTFOUND;;

type genesis = {
	hash		: Hash.t;
	version		: int32;
	prev_block	: Hash.t;
	merkle_root : Hash.t;
	time		: float;
	bits		: uint32;
	nonce		: uint32;
};;


type t = { 
	block_size		: int;
	genesis			: genesis;
	magic				: int;
	port				: int;
	seeds				: string list;
	network			: e;
	checkpoints	: (int * Hash.t) list;
	prefixes		: Address.prefix;
};;



let of_network n =
	match n with
	| BTC -> 
		{ 
			block_size = 1000000;
			network	= BTC;
			prefixes = { pubkeyhash = 0x00; scripthash = 0x05; };

			genesis = { 
				hash		= "000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f";
				merkle_root	= "4a5e1e4baab89f3a32518a88c31bc87f618f76673e2cc77ab2127b7afdeda33b";
				prev_block 	= Hash.zero;
				nonce		= Uint32.of_int 2083236893;
				time		= 1231006505.0;
				bits		= Uint32.of_int 0x1d00ffff;
				version 	= Int32.of_int 1;
			};

			port	= 8333;
			magic	= 0xD9B4BEF9;
			seeds	= [ 
				(*"seed.bitcoin.sipa.be";*) 
				"dnsseed.bluematt.me"; 
				"dnsseed.bitcoin.dashjr.org"; 
				"seed.bitcoinstats.com"; 
				"bitseed.xf2.org"
			];
			checkpoints = [
				 11111, "0000000069e244f73d78e8fd29ba2fd2ed618bd6fa2ee92559f542fdb26e7c1d";
				 33333, "000000002dd5588a74784eaa7ab0507a18ad16a236e7b1ce69f00d7ddfb5d0a6";
				 74000, "0000000000573993a3c9e41ce34471c079dcf5f52a0e824a81e7f953b8661a20";
				105000, "00000000000291ce28027faea320c8d2b054b2e0fe44a773f3eefb151d6bdc97";
				134444, "00000000000005b12ffd4cd315cd34ffd4a594f430ac814c91184a0d42d2b0fe";
				168000, "000000000000099e61ea72015e79632f216fe6cb33d7899acb35b75c8303b763";
				193000, "000000000000059f452a5f7340de6682a977387c17010ff6e6c3bd83ca8b1317";
				210000, "000000000000048b95347e83192f69cf0366076336c639f9b7228e9ba171342e";
				216116, "00000000000001b4f4b433e81ee46494af945cf96014816a4e2370f11b23df4e";
				225430, "00000000000001c108384350f74090433e7fcf79a606b8e797f065b130575932";
				250000, "000000000000003887df1f29024b06fc2200b55f8af8f35453d7be294df2d214";
				279000, "0000000000000001ae8c72a0b0c301f67e3afca10e819efa9041e458e9bd7e40";
				295000, "00000000000000004d9b4ef50f0f9d686fd69db2e03af35a100370c64632a983";
				478559, "00000000000000000019f112ec0a9982926f1258cdcc558dd7c3b7e5dc7fa148";
			];
		}
		| BCH -> 
		{ 
			block_size = 8000000;
			network	= BCH;
			prefixes = { pubkeyhash = 0x00; scripthash = 0x05; };
			
			genesis = { 
				hash		= "000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f";
				merkle_root	= "4a5e1e4baab89f3a32518a88c31bc87f618f76673e2cc77ab2127b7afdeda33b";
				prev_block 	= Hash.zero;
				nonce		= Uint32.of_int 2083236893;
				time		= 1231006505.0;
				bits		= Uint32.of_int 0x1d00ffff;
				version 	= Int32.of_int 1;
			};

			port	= 8333;
			magic	= 0xD9B4BEF9;
			seeds	= [ 
				"seed.bitcoinabc.org"; 
				"seed-abc.bitcoinforks.org"; 
				"btccash-seeder.bitcoinunlimited.info"; 
				"seed.bitprim.org"; 
				"seed.deadalnix.me"; 
				"seeder.criptolayer.net"
			];
			checkpoints = [
				 11111, "0000000069e244f73d78e8fd29ba2fd2ed618bd6fa2ee92559f542fdb26e7c1d";
				 33333, "000000002dd5588a74784eaa7ab0507a18ad16a236e7b1ce69f00d7ddfb5d0a6";
				 74000, "0000000000573993a3c9e41ce34471c079dcf5f52a0e824a81e7f953b8661a20";
				105000, "00000000000291ce28027faea320c8d2b054b2e0fe44a773f3eefb151d6bdc97";
				134444, "00000000000005b12ffd4cd315cd34ffd4a594f430ac814c91184a0d42d2b0fe";
				168000, "000000000000099e61ea72015e79632f216fe6cb33d7899acb35b75c8303b763";
				193000, "000000000000059f452a5f7340de6682a977387c17010ff6e6c3bd83ca8b1317";
				210000, "000000000000048b95347e83192f69cf0366076336c639f9b7228e9ba171342e";
				216116, "00000000000001b4f4b433e81ee46494af945cf96014816a4e2370f11b23df4e";
				225430, "00000000000001c108384350f74090433e7fcf79a606b8e797f065b130575932";
				250000, "000000000000003887df1f29024b06fc2200b55f8af8f35453d7be294df2d214";
				279000, "0000000000000001ae8c72a0b0c301f67e3afca10e819efa9041e458e9bd7e40";
				295000, "00000000000000004d9b4ef50f0f9d686fd69db2e03af35a100370c64632a983";
				478559, "000000000000000000651ef99cb9fcbe0dadde1d424bd9f15ff20136191a5eec";
			];
		}
	| XTN -> 
		{ 
			block_size = 1000000;
			network	= XTN;
			prefixes = { pubkeyhash = 0x6f; scripthash = 0xc4; };
			
			genesis = { 
				hash		= "000000000933ea01ad0ee984209779baaec3ced90fa3f408719526f8d77f4943";
				merkle_root	= "4a5e1e4baab89f3a32518a88c31bc87f618f76673e2cc77ab2127b7afdeda33b";
				prev_block 	= Hash.zero;
				nonce		= Uint32.of_int 414098458;
				time		= 1296688602.0;
				bits		= Uint32.of_int 0x1d00ffff;
				version 	= Int32.of_int 1;
			};

			port	= 18333;
			magic	= 0x0709110B;
			seeds	= [ 
				(*"testnet-seed.bitcoin.schildbach.de";*)
				"testnet-seed.alexykot.me"; 
				"testnet-seed.bluematt.me";
				"testnet-seed.bitcoin.petertodd.org";
			];
			checkpoints = [
				 11111, "0000000028502288da2fa584d24403536a3755351fd53a39f8e9d48d1709fc0e";
				 33333, "000000002976ec50a8dcbff1fa44236d757ce488f5f0ac5e48992e3ce38c2a11";
				 74000, "0000000069a561a7eff87256bf3e3e8b58163814a9de11f30578b5a612fea38c";
				105000, "000000000849648bee6166118eef581eef28cbf461db4bc00d79886616d49f15";
				134444, "000000000f7b38f7a64d121cb59595e2fa2f43987d8ec0ddea4c3d8906e8069a";
				168000, "00000000d472a872d649cb9af283d969fc5c768f574d24d5ef50943037886402";
				193000, "00000000fac2815809d61a5b9dded4f79f99eef434d4f7c75971d914871b137d";
				210000, "00000000cedea3912d480ed573fc7200ad8c44b0d98aff6b829af2b554543632";
				216116, "0000000000602ef6a29067da9f71193ba71f906da159753713a8f28204a96ee1";
				295000, "000000000003e5a235fe2062b19f6c9d63183fadccbd10027bad1056a24e1b0d";				
			];
		}
	| _ -> failwith "Not available"
;;


let name_of_network n =
	match n with 
	| BTC -> "Bitcoin mainnet"
	| XTN -> "Bitcoin testnet"
	| BCH -> "Bitcoincash mainnet"
	| _ -> ""
;;

let abbr_to_network n =
	match n with 
	| "BTC" -> BTC
	| "XTN" -> XTN
	| "BCH" -> BCH
	| _ -> NOTFOUND
;;
