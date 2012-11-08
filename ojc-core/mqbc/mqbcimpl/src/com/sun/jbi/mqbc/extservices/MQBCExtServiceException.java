/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)MQBCExtServiceException.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.mqbc.extservices;

/**
 */
/**
 * Delegate-like class for MQException.  It does nothing special and has no
 * reason for being other than to hide direct references to com.ibm.mq.*
 * classes: there are runtime contexts that use our classes that have
 * implementations that need to throw com.ibm.mq.MQException, but do not have
 * access to the WebSphere MQ Java base class library.
 *
 */
public class MQBCExtServiceException
        extends RuntimeException {
    
    // This class necessarily must NOT be a checked exception.
    //
    // The message/OTD classes of the previus 5.0 MQ Series eWay extended
    // MQ classes directly, but this re-write must hide this inheritance
    // to be able to not have to package com.ibm.mq.jar with the eWay.
    //
    // To accompish this, the message/OTD is now built from Interfaces, not
    // Classes, but these Interfaces and the Classes must remain compatible with
    // the existing collabs that were built with the previous eWay
    // (i.e., they must be compilable with minimal changes, with this eWay).
    //
    // So, where before we had:
    //
    //     // Exposed to the eWay OTD builder
    //     SomeClass extends MQClass {
    //         public void Foo() throws MQException ... // overridden method
    //     }
    //
    //
    // We instead have, now:
    //
    //     // Exposed to the eWay OTD builder
    //     SomeInterface {
    //         public void Foo() throws MyMQException
    //     }
    //
    //     SomeClass extends MQClass implements SomeInterface {
    //         public void Foo() throws MyMQException {
    //             try {
    //                 super.Foo();
    //             }
    //             catch (MQException) {
    //                 throws new MyMQException...
    //             }
    //         }
    //     }
    //
    // If this class was a checked exception, this scheme would not work
    // because SomeInterface.Foo's signature would conflict with
    // MQClass.Foo.
    
    public MQBCExtServiceException(Throwable e) {
        super(e.getMessage(), e);
        
        isMQException = com.ibm.mq.MQException.class.isAssignableFrom(e.getClass());
        if (isMQException) {
            
            com.ibm.mq.MQException  mqe = (com.ibm.mq.MQException) e;
            
            mMessage = mqe.getMessage();
            mCause = mqe.getCause();
            completionCode = mqe.completionCode;
            reasonCode = mqe.reasonCode;
            exceptionSource = mqe.exceptionSource;
        }  
    }
    
    public boolean isMQException() {
        return isMQException;
    }
    
    public MQBCExtServiceException(String msg)
    {
     mMessage = msg;
    }
    public String getMessage() {
        return mMessage;
    }
    
    public Throwable getCause() {
        return mCause;
    }
    
    private String mMessage="";
    private Throwable mCause;
    private boolean isMQException;
    
    public int completionCode=0;
    public int reasonCode=0;
    public Object exceptionSource;
}
