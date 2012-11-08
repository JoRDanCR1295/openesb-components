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
 * @(#)NewInstanceFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.crl.mep.impl;

import java.util.logging.Logger;

import com.sun.jbi.crl.util.I18n;

/**
 * Utility factory class.
 * 
 * @author Kevan Simpson
 */
public class NewInstanceFactory {
    private static Logger mLogger = Logger.getLogger(NewInstanceFactory.class.getName());
    
    private Class mType = null;
    
    public NewInstanceFactory(Class type) {
        mType = type;
    }
    
    protected Object newInstance(Class type) throws InstantiationException,
                                                    IllegalAccessException {
        Object result = null;
        try {
            result = type.newInstance();
        }
        catch (InstantiationException ie) {
            // interface or abstract class
            mLogger.warning(
            		I18n.loc("CRL-6041: Failed to instantiate \"{0}\": {1}", 
            				 String.valueOf(getType().getName()), ie.getMessage()));
            throw ie;
        }
        catch (IllegalAccessException iae) {
            mLogger.warning(
            		I18n.loc("CRL-6042: Illegal access creating type \"{0}\": {1}", 
            				 String.valueOf(getType().getName()), iae.getMessage()));
            throw iae;
        }
        
        return result;
    }

    protected Class getType() {
        return mType;
    }
    
    protected void setType(Class type) {
        mType = type;
    }
}
