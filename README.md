<h1>CaesarChiffre</h1>
Just for Fun Project: Algorithm for encrypting and decrypting text strings using a simple Caesar Chiffre (https://en.wikipedia.org/wiki/Caesar_cipher) . Can handle numbers and special characters to a limited degree. Numbers are translated to words (e.g. 32 becomes "thirtytwo") and then encrypted. Special Characters are not encrypted and remain plaintext.

---

<h3>Example</h3>

<h4>Plaintext:</h4> 

"Contrary to popular belief, Lorem Ipsum is not simply random 
It has roots in a piece of classical Latin literature from 45 BC,
making it over 2000 years old. Richard McClintock, a Latin professor
at Hampden-Sydney College in Virginia, looked up one of the more
obscure Latin words, consectetur, from a Lorem Ipsum passage,
and going through the cites of the word in classical literature,
discovered the undoubtable source. Lorem Ipsum comes from sections
1.10.32 and 1.10.33 of \"de Finibus Bonorum et Malorum\" 
(The Extremes of Good and Evil) by Cicero, written in 45 BC. This
book is a treatise on the theory of ethics, very popular during the Renaissance.
The first line of Lorem Ipsum, \"Lorem ipsum dolor sit amet..\",
comes from a line in section 1.10.32."

<h4>Encrypted with a shift of 4:</h4> 

gsrxvevc xs tstypev fipmij, psviq mtwyq mw rsx wmqtpc verhsq
mx lew vssxw mr e tmigi sj gpewwmgep pexmr pmxivexyvi jvsq jsvxcjmzi fg,
qeomrk mx sziv xasxlsywerh cievw sph. vmglevh qggpmrxsgo, e pexmr tvsjiwwsv
ex leqthir-wchric gsppiki mr zmvkmrme, pssoih yt sri sj xli qsvi
sfwgyvi pexmr asvhw, gsrwigxixyv, jvsq e psviq mtwyq tewweki
, erh ksmrk xlvsykl xli gmxiw sj xli asvh mr gpewwmgep pmxivexyvi,
hmwgszivih xli yrhsyfxefpi wsyvgi. psviq mtwyq gsqiw jvsq wigxmsrw
sri.xir.xlmvxcxas erh sri.xir.xlmvxcxlvii sj \"hi jmrmfyw fsrsvyq ix qepsvyq\"
(xli ibxviqiw sj kssh erh izmp) fc gmgivs, avmxxir mr jsvxcjmzi fg. xlmw
fsso mw e xviexmwi sr xli xlisvc sj ixlmgw, zivc tstypev hyvmrk xli viremwwergi.
xli jmvwx pmri sj psviq mtwyq, \"psviq mtwyq hspsv wmx eqix..\",
gsqiw jvsq e pmri mr wigxmsr sri.xir.xlmvxcxas.

<h4>Decrypted again with a shift of -4:</h4>

"contrary to popular belief, lorem ipsum is not simply random
it has roots in a piece of classical latin literature from fortyfive bc,
making it over twothousand years old. richard mcclintock, a latin professor
at hampden-sydney college in virginia, looked up one of the more
obscure latin words, consectetur, from a lorem ipsum passage,
and going through the cites of the word in classical literature,
discovered the undoubtable source. lorem ipsum comes from sections
one.ten.thirtytwo and one.ten.thirtythree of \"de finibus bonorum et malorum\"
(the extremes of good and evil) by cicero, written in fortyfive bc. this
book is a treatise on the theory of ethics, very popular during the renaissance
. the first line of lorem ipsum, \"lorem ipsum dolor sit amet..\",
comes from a line in section one.ten.thirtytwo."
