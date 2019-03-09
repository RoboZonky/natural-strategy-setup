# Rezervační systém v RoboZonky

Zonky zavedlo koncem roku 2018 tzv. [rezervační systém](https://zonky.cz/rezervace-investic/).
RoboZonky 5.1.0 přináší plnou podporu pro tento systém.
Pro jeho využívání v robotu nepotřebujete mobilní aplikaci - pokud ji přesto používáte, doporučujeme tam vypnout notifikace o nových rezervacích, nicméně na funkci robota to nemá vliv.

## Režimy rezervačního systému
RoboZonky dává uživatelům na výběr, jak se má chovat.
Všechny funkce primárního i sekundárního tržiště budou i nadále bez potíží fungovat nezávisle na režimu rezervačního systému.

### Ignorování rezervačního systému
V tomto režimu robot nebude s rezervačním systémem vůbec komunikovat.
Můžete ho mít nastavený podle libosti, a přes mobilní aplikaci si ho řídit sami.

### Kontrola rezervačního systému
V tomto režimu bude robot respektovat nastavení rezervačního systému, která jste provedli v mobilní aplikaci Zonky.
Nové rezervace podle těchto nastavení bude pravidelně kontrolovat a přijímat ty z nich, které odpovídají robotově investiční strategii.
Rezervace, které neodpovídají strategii, nebudou odmítnuty a pokud je uživatel neschválí ručně, časem vyprší.
