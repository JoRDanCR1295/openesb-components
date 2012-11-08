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
 * @(#)GeneratorException.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.swift.xsdbuilder;

/**
 * Represents any exception encountered during generation of XSDs.
 *  
 * @author Jun Xu
 */
public class GeneratorException extends Exception {

    private static final long serialVersionUID = 1L;

    public GeneratorException() {
        super();
    }

    public GeneratorException(String arg0, Throwable arg1) {
        super(arg0, arg1);
    }

    public GeneratorException(String arg0) {
        super(arg0);
    }

    public GeneratorException(Throwable arg0) {
        super(arg0);
    }
}
