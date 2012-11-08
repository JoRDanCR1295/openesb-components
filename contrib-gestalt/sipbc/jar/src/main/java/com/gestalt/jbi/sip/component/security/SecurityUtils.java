package com.gestalt.jbi.sip.component.security;


/**
 * Interface defining methods used to handle password encryption
 */
public interface SecurityUtils {
    public String encrypt(String plaintext) throws Exception;

    public String decrypt(String encrypted) throws Exception;
}
