/*
 * BEGIN_HEADER - DO NOT EDIT
 *
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-esb.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-esb.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)classname.java
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.component.jbiext;

import java.lang.reflect.Method;
import javax.jbi.component.ComponentContext;

/**
 *
 *
 * @version      $Revision: 1.2 $
 *
 */
public class KeyStoreUtilClient {

    private Object mKeyStoreUtil;

    public KeyStoreUtilClient(ComponentContext context) {
        if (context != null) {
            try {
                mKeyStoreUtil = invoke(context, "getKeyStoreUtil", null);
            } catch (Exception ex) {
                // Ignore on purpose.  This means that we don't have a proper
                // KeyStoreUtil service provided by our JBI framework
            }
        }
    }

    /**
     * Generates a random key for the alias and stores the key
     * in the key store for the alias.
     *
     * @param        keyName the name of the key
     * @exception    Exception if the key already exists in the KeyStore
     */
    public void addKey(String keyName) throws Exception {
        if (mKeyStoreUtil != null) {
            invoke(mKeyStoreUtil, "addKey", keyName);
        }
    }

    /**
     * Checks to see if a key referenced by keyName exists in the KeyStore
     *
     * @param        keyName the name of the Key
     * @return       true if the key exists in the KeyStore, false otherwise
     * @exception    Exception if any error occurs.
     * @see          
     */
    public boolean hasKey(String keyName) throws Exception {
        if (mKeyStoreUtil != null) {
            Object retVal = invoke(mKeyStoreUtil, "hasKey", keyName);
            return ((Boolean)retVal).booleanValue();
        }
        return false;
    }

    /**
     * Updates an existing alias in the KeyStore when a new randomly generated key.
     *
     * @param        keyName the name of the key
     * @exception    Exception if the keyName does not already exist in
     * the KeyStore
     */
    public void updateKey(String keyName) throws Exception {
        if (mKeyStoreUtil != null) {
            invoke(mKeyStoreUtil, "updateKey", keyName);
        }
    }

    /**
     * Deletes a key from the KeyStore.
     *
     * @param        keyName the name of the key
     * @exception    Exception if the key can't be deleted
     */
    public void deleteKey(String keyName) throws Exception {
        if (mKeyStoreUtil != null) {
            invoke(mKeyStoreUtil, "deleteKey", keyName);
//             Method deleteKey =
//                 mKeyStoreUtil.getClass().getMethod("deleteKey", keyName.getClass());
//             deleteKey.invoke(mKeyStoreUtil, keyName);
        }
    }
 
    /**
     * Lists all key names in the KeyStore
     *
     * @return       the list of key names as a String array
     * @exception    Exception if any error occurs while trying
     * to retrieve the list of key names
     */
    public String[] listKeyNames() throws Exception {
        if (mKeyStoreUtil != null) {
            return (String[])invoke(mKeyStoreUtil, "listKeyNames", null);
        }
        return new String[0];
    }

    /**
     * Retrieves the key as a string
     *
     * @param        keyName the name of the key
     * @return       the key as a plain-text or null if the key doesn't exist
     * @exception    Exception if any error occurs while trying
     * to retrieve the key
     */    
    public String getKey(String keyName) throws Exception {
        if (mKeyStoreUtil != null) {
            return (String)invoke(mKeyStoreUtil, "getKey", keyName);
        }
        return null;
    }

    /**
     * Encrypts a message using the key identified by keyName
     *
     * @param        keyName the name of the key
     * @param        cleartext the byte array that will be encrypted
     * @return       the encrypted byte array
     * @exception    Exception if any error occurs retrieving the
     * key to be used
     */   
    public byte[] encrypt(String keyName, byte[] clearText)
        throws Exception {
        if (mKeyStoreUtil != null) {
            return (byte[])invoke(mKeyStoreUtil, "encrypt", keyName, clearText);
        }
        return clearText;
    }

    /**
     * Decrypts a message using the key identified by keyName
     *
     * @param        keyName the name of the key
     * @param        ciphertext the byte array with the encrypted data
     * @return       the unencrypted byte array
     * @exception    Exception if any error occurs retrieving the
     * key to be used
     */
    public byte[] decrypt(String keyName, byte[] cipherText)
        throws Exception {
        if (mKeyStoreUtil != null) {
            return (byte[])invoke(mKeyStoreUtil,
                                  "decrypt",
                                  keyName,
                                  cipherText);
        }
        return cipherText;
    }

    /**
     * Encrypts a message using the key identified by keyName.  The result
     * is a Base64-encoded string.
     *
     * @param        keyName the name of the key
     * @param        clearText a String representing the message to be encrypted
     * @return       a Base64-encoded string representing the encrypted message
     * @exception    Exception if any error occurs retrieving the
     * key to be used
     */
    public String encrypt(String keyName, String clearText)
        throws Exception {
        if (mKeyStoreUtil != null) {
            return (String)invoke(mKeyStoreUtil, "encrypt", keyName, clearText);
        }
        return clearText;
    }

    /**
     * Decrypts a message using the key identified by keyName.  The second
     * argument must be a Base-64 encoded string
     *
     * @param        keyName the name of the key
     * @param        base64EncodedCipherText a Base-64 Encoded string
     * @return       the decrypted message as a String
     * @exception    Exception if any error occurs retrieving the
     * key to be used
     */
    public String decrypt(String keyName, String base64EncodedCipherText)
        throws Exception {
        if (mKeyStoreUtil != null) {
            return (String)invoke(mKeyStoreUtil, "mKeyStoreUtil", keyName,
                                  base64EncodedCipherText);
        }
        return base64EncodedCipherText;
    }

    /**
     * Encrypts a message using a default key. 
     *
     * @param        keyName the name of the key
     * @param        clearText the byte array that will be encrypted
     * @return       the encrypted byte array
     * @exception    Exception if any error occurs retrieving the
     * key to be used
     */
    public byte[] encrypt(byte[] clearText) throws Exception {
        if (mKeyStoreUtil != null) {
            return (byte[])invoke(mKeyStoreUtil, "encrypt", clearText);

        }
        return clearText;
    }

    /**
     * Decrypts a message using a default key
     *
     * @param        keyName the name of the key
     * @param        ciphertext the byte array with the encrypted data
     * @return       the unencrypted byte array
     * @exception    Exception if any error occurs retrieving the
     * key to be used
     */
    public byte[] decrypt(byte[] cipherText) throws Exception {
        if (mKeyStoreUtil != null) {
            return (byte[])invoke(mKeyStoreUtil, "decrypt", cipherText);
//             Method decrypt =
//                 mKeyStoreUtil.getClass().getMethod("decrypt",
//                                                    cipherText.getClass());
//             Object retVal = decrypt.invoke(mKeyStoreUtil, cipherText);
//             return (byte[])retVal;
        }
        return cipherText;
    }

    /**
     * Encrypts a message using a default key.  The result
     * is a Base64-encoded string.
     *
     * @param        keyName the name of the key
     * @param        clearText a String representing the message to be encrypted
     * @return       a Base64-encoded string representing the encrypted message
     * @exception    Exception if any error occurs retrieving the
     * key to be used
     */
    public String encrypt(String clearText) throws Exception {
        if (mKeyStoreUtil != null) {
            return (String)invoke(mKeyStoreUtil, "encrypt", clearText);
        }
        return clearText;
    }

    /**
     * Decrypts a message using the key identified by keyName.  The second
     * argument must be a Base-64 encoded string
     *
     * @param        keyName the name of the key
     * @param        base64EncodedCipherText a Base-64 Encoded string
     * @return       the decrypted message as a String
     * @exception    Exception if any error occurs retrieving the
     * key to be used
     */
    public String decrypt(String base64EncodedCipherText) throws Exception {
        if (mKeyStoreUtil != null) {
            return (String)invoke(mKeyStoreUtil, "decrypt",
                                  base64EncodedCipherText);
        }
        return base64EncodedCipherText;
    }

    /**
     * 
     *
     * @param        
     * @return       
     * @exception    
     * @see          
     */
    public Object invoke(Object caller,
                         String methodName,
                         Object... args) throws Exception {

        Class[] classArgs = null;
        if (args != null) {
            classArgs = new Class[args.length];
            for (int ii = 0; ii < args.length; ii++) {
                if (args[ii] != null) {
                    classArgs[ii] = args[ii].getClass();
                } else {
                    classArgs[ii] = null;
                }
            }
        }
        Method callerMethod =
            caller.getClass().getMethod(methodName, classArgs);
        Object retVal = callerMethod.invoke(caller,
                                            args);
        return retVal;
    }
}
