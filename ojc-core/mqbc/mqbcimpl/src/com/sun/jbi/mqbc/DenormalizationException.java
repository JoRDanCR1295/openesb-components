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
 */

/*
 * @(#)DenormalizationException.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.mqbc;

/**
 * This exception is raised when the Message Exchange denormalization fails.
 *
 * @author Noel.Ang@sun.com
 */
public class DenormalizationException extends Exception {
    public DenormalizationException(String msg) {
        super(msg);
    }

    public DenormalizationException(String msg, Exception cause) {
        super(msg, cause);
    }
}
