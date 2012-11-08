 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.test.jbi;

import java.util.Locale;
import it.imolinfo.jbi4corba.jbi.Messages;
import junit.framework.TestCase;

public class I18NTest extends TestCase {

    public I18NTest() {
    }

    public I18NTest(String name) {
        super(name);
    }

    public void testTranslation() {
        Messages m = Messages.getMessages(getClass(), Locale.ITALIAN);
        String msg = m.getString("Messages.saluta");

        assertEquals("Traduzione italiana del messaggio fallita",
                     "Ciao a tutti, belli e brutti", msg);
    }

    public void testTranslationWithParams() {
        Messages m = Messages.getMessages(getClass(), Locale.ITALIAN);
        String msg = m.getString("Messages.saluta_3_persone",
                                 new Object[] { "Qui", "Quo", "Qua"});

        assertEquals("Traduzione italiana del messaggio con parametri fallita",
                     "Tanti saluti a Qui, Quo e Qua", msg);
    }

    public void testTranslationForDefaultLocale() {
        Messages m = createMessagesForDefaultLocale();
        String msg = m.getString("Messages.saluta");

        assertEquals("Traduzione italiana del messaggio fallita",
                     "Ciao a tutti, belli e brutti", msg);
    }

    public void testTranslationWithParamsForDefaultLocale() {
        Messages m = createMessagesForDefaultLocale();
        String msg = m.getString("Messages.saluta_3_persone",
                new Object[] { "Grazia", "Graziella", "grazie al c...o" });

        assertEquals("Traduzione italiana del messaggio con parametri fallita",
                     "Tanti saluti a Grazia, Graziella e grazie al c...o", msg);
    }

    private Messages createMessagesForDefaultLocale() {
        Locale origLocale = Locale.getDefault();

        /*
         * Change the default locale to use a bundle which has an exact matching
         * properties file: we (re)use the italian bundle
         */
        Locale.setDefault(Locale.ITALIAN);
        try {
            return Messages.getMessages(getClass());
        } finally {
            Locale.setDefault(origLocale);
        }
    }

    public void testTranslationWithUnknownLocale() {
        Messages m = createMessagesForUnknownLocale();

        assertEquals("Traduzione del messaggio fallita per Locale non presente",
                     "Bye", m.getString("Messages.saluta"));
    }

    public void testTranslationWithParamsAndUnknownLocale() {
        Messages m = createMessagesForUnknownLocale();
        String msg = m.getString("Messages.saluta_3_persone",
                                 new Object[] { "me", "you", "him" });

        assertEquals("Traduzione del messaggio con parametri fallita per Locale"
                     + " non presente", "I say bye to me, you and him", msg);
    }

    private Messages createMessagesForUnknownLocale() {
        Locale origLocale = Locale.getDefault();

        /*
         * Change the default locale to use a bundle which has not an exact
         * matching properties file
         */
        Locale.setDefault(Locale.JAPAN);
        try {
            return Messages.getMessages(getClass(), Locale.CHINESE);
        } finally {
            Locale.setDefault(origLocale);
        }
    }
}
