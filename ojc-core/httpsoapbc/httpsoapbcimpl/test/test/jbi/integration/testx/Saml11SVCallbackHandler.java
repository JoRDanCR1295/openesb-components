/*
 * Saml11SVCallbackHandler.java
 *
 * Created on September 23, 2008, 2:45 PM
 */

package test.jbi.integration.testx;

import java.io.*;
import java.util.*;
import java.math.BigInteger;

import java.security.KeyStore;

import javax.security.auth.callback.Callback;
import javax.security.auth.callback.CallbackHandler;
import javax.security.auth.callback.UnsupportedCallbackException;

import com.sun.xml.wss.impl.callback.*;

import com.sun.xml.wss.saml.*;

import org.w3c.dom.*;

/** 
 *
 * @author  Sujit Biswas
 * @version
 * 
 * SAML 1.1 Sender Vouches Callback Handler
 */
public  class Saml11SVCallbackHandler implements CallbackHandler {    
    
    private String keyStoreURL;
    private String keyStorePassword;
    private String keyStoreType;
    
    private String trustStoreURL;
    private String trustStorePassword;
    private String trustStoreType;
    
    private KeyStore keyStore;
    private KeyStore trustStore;
    
    private static final String fileSeparator = System.getProperty("file.separator");
    
    private  UnsupportedCallbackException unsupported =
            new UnsupportedCallbackException(null,
            "Unsupported Callback Type Encountered");

    private  static Element svAssertion = null;
    
    public static final String senderVouchesConfirmation =
    "urn:oasis:names:tc:SAML:1.0:cm:sender-vouches";
    
    String glassfish_home = System.getProperty("com.sun.aas.installRoot");
    String client_priv_key_alias="xws-security-client";

    public Saml11SVCallbackHandler() {
        try {

            this.keyStoreURL = glassfish_home + fileSeparator + "domains" + fileSeparator + "domain1" + 
                    fileSeparator + "config" + fileSeparator + "keystore.jks";
            this.keyStoreType = "JKS";
            this.keyStorePassword = "changeit";
                                                                                                                                                             
            this.trustStoreURL = glassfish_home + fileSeparator + "domains" + fileSeparator + "domain1" + 
				fileSeparator + "config" + fileSeparator + "cacerts.jks";
            this.trustStoreType = "JKS";
            this.trustStorePassword = "changeit";            
            initKeyStore();
            initTrustStore();			
        }catch(Exception e) {
            e.printStackTrace();
            throw new RuntimeException(e);
        }
    }

    public void handle(Callback[] callbacks) throws IOException, UnsupportedCallbackException {
        for (int i=0; i < callbacks.length; i++) {
            if (callbacks[i] instanceof SAMLCallback) {
                try{
                    SAMLCallback samlCallback = (SAMLCallback)callbacks[i];
                    
                    
                    System.out.println("========================= Printing out runtime properties...");
                    System.out.println(samlCallback.getRuntimeProperties());
                    System.out.println("========================= Done printing out runtime properties...");
                  
                    
                    
                    samlCallback.setConfirmationMethod(samlCallback.SV_ASSERTION_TYPE);
                    if (samlCallback.getConfirmationMethod().equals(samlCallback.SV_ASSERTION_TYPE)){
                            samlCallback.setAssertionElement(createSVSAMLAssertion());                            
                            svAssertion=samlCallback.getAssertionElement();
                    }else{
                            throw new Exception("SAML Assertion Type is not matched.");
                    }
                }catch(Exception ex){
                        ex.printStackTrace();
                }
            } else {
                throw unsupported;
            }
        }
    }
    
    private static Element createSVSAMLAssertion() {
        Assertion assertion = null;
        try {
            // create the assertion id
            String assertionID = String.valueOf(System.currentTimeMillis());
            String issuer = "CN=Assertion Issuer,OU=AI,O=Assertion Issuer,L=Waltham,ST=MA,C=US";
            
            
            GregorianCalendar c = new GregorianCalendar();
            long beforeTime = c.getTimeInMillis();
            // roll the time by one hour
            long offsetHours = 60*60*1000;

            c.setTimeInMillis(beforeTime - offsetHours);
            GregorianCalendar before= (GregorianCalendar)c.clone();
            
            c = new GregorianCalendar();
            long afterTime = c.getTimeInMillis();
            c.setTimeInMillis(afterTime + offsetHours);
            GregorianCalendar after = (GregorianCalendar)c.clone();
            
            GregorianCalendar issueInstant = new GregorianCalendar();
            // statements
            List statements = new LinkedList();


            SAMLAssertionFactory factory = SAMLAssertionFactory.newInstance(SAMLAssertionFactory.SAML1_1);

            NameIdentifier nmId =
            factory.createNameIdentifier(
            "CN=SAML User,OU=SU,O=SAML User,L=Los Angeles,ST=CA,C=US",
            null, "urn:oasis:names:tc:SAML:1.1:nameid-format:X509SubjectName");

            SubjectConfirmation scf =
            factory.createSubjectConfirmation("urn:oasis:names:tc:SAML:1.0:cm:sender-vouches");
           
 
            Subject subj = factory.createSubject(nmId, scf);
           
            List attributes = new LinkedList();

            List attributeValues = new LinkedList();
            attributeValues.add("ATTRIBUTE1");
            attributes.add( factory.createAttribute(
                "attribute1",
                "urn:com:sun:xml:wss:attribute",
                 attributeValues));

            statements.add(
            factory.createAttributeStatement(subj, attributes));
            
            Conditions conditions = factory.createConditions(before, after, null, null, null);
            
            assertion = factory.createAssertion(assertionID, issuer, issueInstant,
            conditions, null, statements);
            assertion.setMajorVersion(BigInteger.ONE);
            assertion.setMinorVersion(BigInteger.ONE);
            return assertion.toElement(null);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
    
    private void initKeyStore() throws IOException {
        try {
            keyStore = KeyStore.getInstance(keyStoreType);
            keyStore.load(new FileInputStream(keyStoreURL), keyStorePassword.toCharArray());
        } catch (Exception e) {
            throw new IOException(e.getMessage());
        }
    }
    
    private void initTrustStore() throws IOException {
        try {
            trustStore = KeyStore.getInstance(trustStoreType);
            trustStore.load(new FileInputStream(trustStoreURL), trustStorePassword.toCharArray());
        } catch (Exception e) {
            throw new IOException(e.getMessage());
        }
    }    
}
