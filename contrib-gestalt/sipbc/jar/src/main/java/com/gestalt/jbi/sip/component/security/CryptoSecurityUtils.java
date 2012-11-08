/**
 *   sip-binding-component - SIP Binding Component
 *
 *   Copyright (C) 2007 Gestalt, LLC. All Rights Reserved.
 *   http://www.gestalt-llc.com/
 *
 *   This library is free software; you can redistribute it and/or
 *   modify it under the terms of the GNU Lesser General Public
 *   License version 2.1 as published by the Free Software Foundation.
 *
 *   This library is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *   Lesser General Public License for more details.
 *
 *   You should have received a copy of the GNU Lesser General Public
 *   License along with this library; if not, write to the Free Software
 *   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301 USA
 */
package com.gestalt.jbi.sip.component.security;

import com.sun.org.apache.xerces.internal.impl.dv.util.Base64;
import com.sun.jbi.internationalization.Messages;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;

import java.security.GeneralSecurityException;
import java.security.KeyStore;
import java.util.logging.Logger;
import java.util.logging.Level;

import javax.crypto.Cipher;
import javax.crypto.KeyGenerator;
import javax.crypto.SecretKey;
import javax.crypto.spec.SecretKeySpec;


public class CryptoSecurityUtils implements SecurityUtils {
    private static final Logger log = Messages.getLogger(CryptoSecurityUtils.class);
    private static Messages messages = Messages.getMessages(CryptoSecurityUtils.class);
    private Cipher encipher;
    private Cipher decipher;
    private String algorithm = "Blowfish";
    private File KEY_STORE_FILE = new File("sipbc.jks");
    private final String KEYSTORE_TYPE = "JCEKS";
    private final char[] STORE_PASSWD = "p4ssw0rd".toCharArray();
    private final char[] KEY_PASSWD = "k3yp4ssw0rd".toCharArray();
    private final String KEY_ALIAS = "SIPBC";

    public CryptoSecurityUtils(String algorithm) throws Exception {
        this.algorithm = algorithm;
        init();
    }

    public CryptoSecurityUtils() throws Exception {
        init();
    }

    private void init() throws Exception {
        if (log.isLoggable(Level.FINE)) {
            log.log(Level.FINE,"Using Algorithm: " + algorithm);
        }
        encipher = Cipher.getInstance(algorithm);
        decipher = Cipher.getInstance(algorithm);

        SecretKey skey;
        KeyStore ks = KeyStore.getInstance(KEYSTORE_TYPE);

        if (KEY_STORE_FILE.exists()) {
            if (log.isLoggable(Level.FINER)) {
                log.log(Level.FINER, "Using the existing key store file.");
            }
            ks.load(new FileInputStream(KEY_STORE_FILE), STORE_PASSWD);
            skey = (SecretKey) ks.getKey(KEY_ALIAS, KEY_PASSWD);
        } else {
            if (log.isLoggable(Level.FINER)) {
                log.log(Level.FINER, "Unable to locate an existing key store file.  The file will be created.");
            }
            KeyGenerator kgen = KeyGenerator.getInstance(algorithm);
            skey = kgen.generateKey();

            ks.load(null, STORE_PASSWD);

            ks.setKeyEntry(KEY_ALIAS, skey, KEY_PASSWD, null);

            FileOutputStream fos = new FileOutputStream(KEY_STORE_FILE);
            ks.store(fos, STORE_PASSWD);
            fos.close();
        }

        byte[] raw = skey.getEncoded();
        SecretKeySpec skeySpec = new SecretKeySpec(raw, algorithm);
        encipher.init(Cipher.ENCRYPT_MODE, skeySpec);
        decipher.init(Cipher.DECRYPT_MODE, skeySpec);
    }

    public String decrypt(String password) throws GeneralSecurityException {
        byte[] encrypted = decipher.doFinal(Base64.decode(password));

        return getUTF8String(encrypted);
    }

    public String encrypt(String password) throws GeneralSecurityException {
        byte[] encrypted = encipher.doFinal(getUTF8Bytes(password));

        return Base64.encode(encrypted);
    }

    private byte[] getUTF8Bytes(String data) {
        byte[] bytes;

        if (data == null) {
            bytes = new byte[0];
        } else {
            try {
                bytes = data.getBytes("UTF-8");
            } catch (java.io.UnsupportedEncodingException uee) {
                log.log(Level.WARNING,messages.getString("SIPBC-W00500.exceptionPreparingForEncryption"),uee);
                bytes = data.getBytes();
            }
        }

        return bytes;
    }

    private String getUTF8String(byte[] data) {
        try {
            return new String(data, "UTF-8");
        } catch (java.io.UnsupportedEncodingException uee) {
            log.log(Level.WARNING,messages.getString("SIPBC-W00501.exceptionExtractingAUTF-8StringFromTheBytesProvided"),uee);
            return new String(data);
        }
    }

    /**
     * Deletes keystore file
     */
    public void cleanup() {
        if (log.isLoggable(Level.FINE)) {
            log.log(Level.FINE,"Removing Key Store File");
        }
        KEY_STORE_FILE.delete();
    }
}
