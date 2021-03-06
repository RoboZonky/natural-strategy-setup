# Změny formátu strategie

## RoboZonky 6.0.0

Přibyly tři nové podmínky

| Podmínka            | Poznámka | Lze použít při tvorbě pravidel pro .. |
| ------------------- | -------- | ------------------------------------- |
| Aktuální doba po splatnosti | Ve dnech |  Nákup/prodej participací |
| Doba od posledního dne po splatnosti | Ve dnech  | Nákup/prodej participací|
| Nejdelší doba po splatnosti | Ve dnech | Nákup/prodej participací|


## RoboZonky 5.7.0

Došlo k zásadní změně v sekci `Výše investice`

### Dříve
- sekce `Výše investice` sloužila ke konfiguraci investic pro primární i sekundární tržiště
- výše investic jste mohli nastavit pomocí rozsahu (minimum až maximum)

### Nyní
- konfigurace pro primární a sekundární tržiště byly rozděleny (`Primární tržiště - výše investice ` / `Sekundární tržiště - výše nákupu`)
- již není možné používat rozsahy, ale částky jsou vždy určeny pouze jedním číslem:
   - na primárním tržišti toto číslo určuje fixní částku, která bude investována do každé půjčky
   - na sekundárním tržišti toto číslo určuje horní hranici intervalu, který vždy začíná od 1 Kč
 
:warning: Pokud jste v původní strategii měli definovánu výši investic pomocí intervalu (X až Y), byla při převodu na
 novou konfiguraci použita pouze horní hranice (Y). Výrazně doporučujeme nové nastavení překontrolovat / dle potřeby upravit.

Přidali jsme několik nových podmínek, které můžete použít při tvorbě pravidel

| Podmínka            | Poznámka | Lze použít při tvorbě pravidel pro .. |
| ------------------- | -------- | ------------------------------------- |
| Dosažený výnos      | V procentech | Prodej participací |
| Původní délka úvěru | V měsících   | Nákup/prodej participací|
| Sleva               | V procentech původní částky | Nákup/prodej participací|
| Historie splácení   | Určuje zda půjčka nikdy nebyla / někdy byla / nyní je v delikvenci | Nákup/prodej participací|

V sekci `Pravidla prodeje` byla přidána volba `Prodávat všechny participace bez poplatku a slevy, které odpovídají filtrům tržiště.`

V sekci `Obecná nastavení` bylo odstraněno nastavení `Maximální podíl investice`.
Pokud jste měli zvoleno `Investovat maximálně X % výše úvěru.`, bylo toto nastavení z Vaší strategie odstraněno.
Od nynějška bude robot investovat bez ohledu na procentuální podíl investice.

## RoboZonky 5.6.0

Byla zjednodušena konfigurace v sekci `Struktura portfolia`. Pokud zvolíte `mnou definované` portfolio, 
od nynějška budete moci nastavit pro každou rizikovou kategorii pouze **jednu hodnotu** požadovaného procentuálního podílu 
(např.: "Prostředky úročené 2,99 % p.a. mají tvořit **3 %** aktuální zůstatkové částky.")
V minulosti bylo možné nastavit i **rozsah hodnotnot** (např.: "Prostředky úročené 2,99 % p.a. mají tvořit **3 až 5 %** aktuální zůstatkové částky.").
Pokud jste ve své strategii měli nastaveno `mnou definované` portfolio s použitím rozsahu hodnot (např.: 3 až 5 %),
 bylo změněno na použití horní hranice (v uvedeném příkladu to je 5%).

Stejně jako v minulosti může i nyní součet procentuálních podílu přesahovat 100%.
Přesto Vám doporučujeme držet se co nejblíže součtu 100%.
Hodnoty výrazně přesahující 100% mohou vést ke zdánlivě nepředvídatelnému chování robota při sestavování portfolia.

## RoboZonky 5.4.0

Po zavedení nové evropské směrnice [PSD2](https://cs.wikipedia.org/wiki/Sm%C4%9Brnice_PSD2) odstranilo 
Zonky ze svých programových rozhraní informace o disponibilním zůstatku.
Tyto informace RoboZonky používal pro funkci investování na základě disponibilního zůstaku.
Tuto funkci jsme proto byli nuceni odstranit a spolu s ní i sekci `Obecná nastavení > Disponibilní zůstatek` ve webovém konfiguračním nástroji.
Pokud jste měli ve své strategii zapnuto nastavení `Investovat pouze pokud disponibilní zůstatek přesáhne X Kč.`,
 bylo z vaší strategie odstraněno. RoboZonky tedy odteď bude investovat vždy bez ohledu na disponibilní zůstatek.

## RoboZonky 5.3.0

Odstranili jsme podporu pro potvrzování investic mobilem.
Pokud jste jej měli zapnutou, byla z vaší strategie odstraněna.

## RoboZonky 5.1.0

Začátkem roku 2019 odstranilo Zonky koncept ratingu půjček.
V důsledku toho ztratily smysl všechny nastavení strategie na ratingu půjček založené.
Ve verzi RoboZonky 5.1.0 jsme proto byli nuceni provést **tři zpětně nekompatibilním změny** formátu strategie:

1. Potvrzování investic mobilem již není založeno na podmínce podle ratingu půjček, nýbrž na podmínce podle míry úročení půjček.
Pokud jste v původní strategii měli nastaveno potvrzování investic mobilem, bylo toto nastavení odstraněno.
Můžete si ho nastavit znovu s využitím nové podmínky založené na úročení půjček.

2. Pokud jste v pravidlech měli podmínky podle ratingu půjček, byly odstraněny.
Chcete-li dosáhnout podobného výsledku, vytvořte si nová pravidla s použitím podmínky podle míry úročení.

3. Podmínky podle míry úročení půjček byly zpřísněny.
V přechozích verzích jsme umožňovali nastavit úrok na libovolnou hodnotu mezi 0 a 100%.
Protože však míry úročení půjček na Zonky jsou pevně dané (můžou být pouze 3.99, 4.99, 5.99, 8.49, 10.99, 13.49, 15.49 nebo 19.99)
omezili jsme formulář pouze na tyto povolené hodnoty.
Pokud jste ve své strategii měli podmínky specifikující úrok, byly odstraněny.
Vytvořte si je znovu s použitím povolených hodnot.

Přibylo i několik **nových funkci**:
1. Podmínky založené na _výši úvěru_ lze nyní používat i pro filtrování participací (dříve bylo možno takové podmínky použít jen pro filtrování půjček)
2. V pravidlech prodeje i nákupu přibyla možnost přidávat podmínky založené na _optimálním výnosu_ a _výši měsíční splátky_.
3. V sekci _Obecná nastavení_ nyní můžete zapnout [podporu pro rezervační systém](ReservationSystem.md).
