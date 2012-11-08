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
 * @(#)ClassLoaderManager.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder;

/**
 * Encoder framework depends on a ClassLoader to instantiate a certain type of
 * encoder instance. The way to get the ClassLoader depends on the package
 * composition and the ClassLoader structure of the real runtime where the
 * encoder framework is installed on.
 *
 * A real use of the encoder framework is responsible to provide the ClassLoader
 * by providing a implementation of this interface. The implementation class
 * name should be configured in META-INF/services/
 * com.sun.encoder.ClassLoaderManager, which is loaded by the ClassLoader of
 * class <code>com.sun.encoder.EncoderFactory</code>.  Absence of this
 * configuration means the ClassLoader of class 
 * <code>com.sun.encoder.EncoderFactory</code> will be used by the encoder
 * framework to instantiate encoders.
 *
 * @author Jun Yang
 */
public interface ClassLoaderManager {
    
    /**
     * Returns a ClassLoader which can discover all encoder providers using
     * the standard service provider mechanism.
     * @return a ClassLoader which can discover all encoder providers using
     * the standard service provider mechanism.
     */
    ClassLoader getEncoderClassLoader();
}
