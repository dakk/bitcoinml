OPAM_DEPENDS="ocamlfind ounit re"
	 
echo "yes" | sudo add-apt-repository ppa:$ppa
sudo apt-get update -qq
sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra opam
export OPAMYES=1
opam init 
opam install bitcoinml.0.2.4
eval `opam config env`
make
make test