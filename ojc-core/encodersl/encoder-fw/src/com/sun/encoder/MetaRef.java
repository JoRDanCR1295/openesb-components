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
 * @(#)MetaRef.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder;

import java.net.URL;

import javax.xml.namespace.QName;

/**
 * Pointer to the metadata in form of XSD. A MetaRef can be constructed
 * with main meta file path or its URL. Imported schema locations specified
 * in the main XSD file should be relative paths to this root meta file.
 *
 * @since 6.0
 * @version
 */
public interface MetaRef {

    /**
     * Gets the path of the main metadata file.  This path should point to an
     * XSD file on the local file system.  If <code>getURL()</code> returns
     * a value other than <code>null</code>, the return value of this method
     * will be ignored.
     *
     * @return the path of the main meta file
     */
    String getPath();

    /**
     * Gets the URL of the main metadata file.  This URL should point to an
     * XSD file somewhere.  If this method returns a value other than
     * <code>null</code>, the return value of <code>getPath()</code> will
     * be ignored.  To load encoder metadata from a jar file, a URL in form
     * "jar:&lt;url&gt;!/{entry}" can be used.
     *
     * @return the URL of the main meta file
     */
    URL getURL();

    /**
     * Gets the QName of the root element.
     *
     * @return the QName of the root element
     */
    QName getRootElemName();
}
