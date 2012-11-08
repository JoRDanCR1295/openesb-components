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
 * @(#)URI.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.uri;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;

/**
 * Corrected version of java.net.URI (Java 1.4.2 version doesn't work correctly!).
 *
 * @author Sun Microsystems
 * @version 
 */
public abstract class URI {
    
    /** Tests if possible scheme is indeed a DOS drive letter.
     * @param   scheme  Scheme to check.
     * @return  <code>true</code> if scheme is a DOS drive.
     */
    protected static boolean isDOSDrive(String scheme) {
        return ((scheme.length() == 1)
            && ((Character.getType(scheme.charAt(0)) == Character.UPPERCASE_LETTER)
                || (Character.getType(scheme.charAt(0)) == Character.LOWERCASE_LETTER)));
    }
    
    /** Gets the scheme portion of the URI.
     * @return  URI schem.
     */
    public abstract String getScheme();
    
    /** Gets the directory portion of the URI.
     * @return  Directory.
     */
    public abstract String getDirectory();
    
    /** Gets the filename portion of the URI.
     * @return  Filename.
     */
    public abstract String getFilename();
    
    /** Tests if the URI is opaque.
     * @return  <code>true</code> if URI is opaque.
     */
    public abstract boolean isOpaque();
    
    /** Tests if the URI is absolute.
     * @return  <code>true</code> if URI is absolute.
     */
    public abstract boolean isAbsolute();
    
    /** Resolves another URI relative to this.
     * @param   str     URI string to resolve.
     * @return  Resolved URI.
     */
    public abstract URI resolve(String str);
    
    /** Resolves another URI relative to this.
     * @param   uri     URI to resolve.
     * @return  Resolved URI.
     */
    public abstract URI resolve(URI uri);
    
    /** Normalizes this URI (gets rid of unnecessary ../ directories).
     * @return  Normalized URI
     */
    public abstract URI normalize();

    /** Relativizes another URI to this.
     * @param   str     URI string to relativize.
     * @return  Relativized URI
     */
    public abstract URI relativize(String str);
    
    /** Relativizes another URI to this.
     * @param   uri     URI to relativize.
     * @return  Relativized URI.
     */
    public abstract URI relativize(URI uri);
    
    /** Forms the URL representation of this URI.
     * @return  URL representation.
     * @throws  MalformedURLException   If cannot be converted to URL.
     */
    public abstract URL toURL() throws MalformedURLException;
    
    /** Forms the File representation of this URI if it is of that scheme.
     * @return  File representation or <code>null</code> if not a file scheme.
     */
    public abstract File toFile();
    
}
