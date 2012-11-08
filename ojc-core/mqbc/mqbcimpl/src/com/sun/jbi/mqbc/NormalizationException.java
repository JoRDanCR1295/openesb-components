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

package com.sun.jbi.mqbc;

/**
 * This exception is raised when the Message Exchange normalization fails.
 *
 * @author Noel.Ang@sun.com
 */
public class NormalizationException extends Exception {
    public NormalizationException(String msg) {
        super(msg);
    }

    public NormalizationException(String msg, Exception cause) {
        super(msg, cause);
    }
}
