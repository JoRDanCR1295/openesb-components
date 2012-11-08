/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package samltokenprocessor;

import com.sun.xml.wss.XWSSecurityException;
import com.sun.xml.wss.saml.SAMLAssertionFactory;
import com.sun.xml.wss.saml.SAMLException;
import com.sun.xml.wss.saml.internal.saml20.jaxb20.AssertionType;
import com.sun.xml.wss.saml.internal.saml20.jaxb20.NameIDType;
import com.sun.xml.wss.saml.internal.saml20.jaxb20.SubjectType;

import java.util.List;
import java.util.Set;

import javax.xml.bind.JAXBElement;
import javax.xml.stream.XMLStreamReader;

import javax.security.auth.Subject;
import javax.security.auth.x500.X500Principal;
import sun.security.x509.AVA;
import sun.security.x509.X500Name;

/**
 *
 * @author mpottlapelli
 */
public class NameProcessor {

    private static final String X509_FORMAT = "urn:oasis:names:tc:SAML:1.1:nameid-format:X509SubjectName";

    public static String extractTokens(Subject subj) {

        try {
            String retValue = "";
            Set<Object> set = subj.getPublicCredentials();
            for (Object obj : set) {
                if (obj instanceof XMLStreamReader) {
                    XMLStreamReader reader = (XMLStreamReader) obj;
                    if (isEmpty(retValue)) {
                        retValue = processStream(reader);
                    } else {
                        retValue = retValue + "," + processStream(reader);
                    }
                }
            }
            return retValue;

        } catch (Exception ex) {
            System.out.println("Error in the subject processing" + ex.getMessage());
            ex.printStackTrace();
        }
        return "";
    }

    public static boolean isEmpty(String s) {
        return ((null == s) || (s.trim().length() == 0));
    }

    public static String processStream(XMLStreamReader reader) throws XWSSecurityException, SAMLException {
        SAMLAssertionFactory factory = SAMLAssertionFactory.newInstance(SAMLAssertionFactory.SAML2_0);
        com.sun.xml.wss.saml.Assertion assertIn = factory.createAssertion(reader);

        if (assertIn instanceof AssertionType) {
            AssertionType assertType = (AssertionType) assertIn;
            SubjectType subject = assertType.getSubject();
            if (subject != null) {
                List<JAXBElement<?>> contents = subject.getContent();
                if (contents != null && !contents.isEmpty()) {
                    for (JAXBElement jaxElem : contents) {
                        if (jaxElem.getValue() instanceof NameIDType) {
                            NameIDType nameId = (NameIDType) jaxElem.getValue();
                            String format = nameId.getFormat();
                            String nameVal = nameId.getValue();

                            // For X509 format the user identifier is extracted, for others content is taken as is
                            String userIdentifier = nameVal;
                            if ((!isEmpty(format)) && (!isEmpty(nameVal))) {
                                if (format.trim().equals(X509_FORMAT)) {
                                    String extractedUID = extract509(nameVal);
                                    if (!isEmpty(extractedUID)) {
                                        userIdentifier = extractedUID;
                                        return userIdentifier;
                                    }
                                } else {
                                        return userIdentifier;
                                }
                            }
                        }
                    }
                }
            }
        }
        return null;
    }

    private static String extract509(String in509) {
        String userVal = null;

        if (!isEmpty(in509)) {
            try {
                X500Principal prin = new X500Principal(in509);
                X500Name name500 = X500Name.asX500Name(prin);
                for (AVA ava : name500.allAvas()) {
                    if (X500Name.userid_oid == ava.getObjectIdentifier()) {
                        userVal = ava.getValueString();
                    } else {
                        return null;
                    }
                }
            } catch (IllegalArgumentException iae) {
                return null;
            }
        }
        return userVal;
    }
}
