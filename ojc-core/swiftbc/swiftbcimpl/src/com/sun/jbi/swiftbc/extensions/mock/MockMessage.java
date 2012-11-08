/*
 * MockMessage.java
 *
 * Created on April 7, 2007, 8:00 PM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package com.sun.jbi.swiftbc.extensions.mock;
import com.sun.jbi.swiftbc.extensions.SwiftEnvelope;
import com.sun.jbi.swiftbc.extensions.SwiftMessage;
import com.sun.jbi.swiftbc.extensions.SwiftLetter;
import com.sun.jbi.swiftbc.extensions.base.SwiftMessageBase;
import javax.xml.namespace.QName;

/**
 *
 * @author Sun Microsystems, Inc.
 */
public class MockMessage extends SwiftMessageBase implements SwiftMessage{
    private SwiftLetter letter;
    private SwiftEnvelope envelope;
    /** Creates a new instance of MockMessage */
    public MockMessage() {
    }

    public void setLetter(SwiftLetter letter) {
        this.letter = letter;
    }

    public void setEnvelope(SwiftEnvelope envelope) {
        this.envelope = envelope;
    }

    public SwiftLetter getLetter() {
        return letter;
    }

    public SwiftEnvelope getEnvelope() {
        return envelope;
    }
}
