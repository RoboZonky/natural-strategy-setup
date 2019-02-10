# Změny formátu strategie

## Robozonky 5.1.0

Začátkem roku 2019 odstranilo Zonky koncept ratingu půjček.
V důsledku toho ztratily smysl všechny nastavení strategie na ratingu půjček založené.
Ve verzi Robozonky 5.1.0 jsme proto byli nuceni provést tři zpětně nekompatibilním změny formátu strategie:

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
