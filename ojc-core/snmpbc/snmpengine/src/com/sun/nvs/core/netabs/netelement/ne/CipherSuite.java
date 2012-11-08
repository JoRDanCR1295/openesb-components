/*
 * Copyright 2005 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package com.sun.nvs.core.netabs.netelement.ne;

/**
 * DOCUMENT ME!
 *
 * @author $author$
 * @version $Revision: 1.2 $
  */
public final class CipherSuite {
    /**
     * DOCUMENT ME!
     */
    public static CipherSuite DES = new CipherSuite("des", "des", "DES Cipher");

    /**
     * DOCUMENT ME!
     */
    public static CipherSuite DES3 = new CipherSuite("3des", "3des", "Triple-DES Cipher");

    /**
     * DOCUMENT ME!
     */
    public static CipherSuite BLOWFISH = new CipherSuite(
            "blowflish", "blowfish", "Blow-fish cipher"
        );

    /**
     * DOCUMENT ME!
     */
    public static CipherSuite TWOFISH = new CipherSuite("twofish", "twofish", "Two-fish cipher");

    /**
     * DOCUMENT ME!
     */
    public static CipherSuite AES_128 = new CipherSuite(
            "aes_128", "aes128", "AES cipher (128 bit)"
        );

    /**
     * DOCUMENT ME!
     */
    public static CipherSuite AES_192 = new CipherSuite(
            "aes_192", "aes192", "AES cipher (192 bit)"
        );

    /**
     * DOCUMENT ME!
     */
    public static CipherSuite AES_256 = new CipherSuite(
            "aes_256", "aes256", "AES cipher (256 bit)"
        );
    private static final CipherSuite[] all = {
            DES, DES3, BLOWFISH, TWOFISH, AES_128, AES_192, AES_256,
        };
    private String cipherName; // Name for UI Display 
    private String cipherId; // TO BE USED by SSH library (internal use)
    private String description; // Description for UI display

    private CipherSuite(String name, String id, String descr) {
        cipherName = name;
        cipherId = id;
        description = descr;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getName() {
        return cipherName;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getDescription() {
        return description;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getId() {
        return cipherId;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String toString() {
        return cipherName;
    }

    /**
     * DOCUMENT ME!
     *
     * @param name DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static CipherSuite getByName(String name) {
        for (int i = 0; i < all.length; i++) {
            if (all[i].cipherName.equalsIgnoreCase(name)) {
                return all[i];
            }
        }

        return null;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String[][] getAllNamesAndDescriptions() {
        String[][] ret = new String[2][all.length];

        for (int i = 0; i < all.length; i++) {
            ret[0][i] = all[i].getName();
            ret[1][i] = all[i].getDescription();
        }

        return ret;
    }
}
