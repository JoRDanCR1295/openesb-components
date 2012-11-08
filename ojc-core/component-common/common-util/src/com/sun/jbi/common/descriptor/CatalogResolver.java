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
 * @(#)CatalogResolver.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.descriptor;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.management.DeploymentException;
import javax.xml.transform.URIResolver;
import org.xml.sax.EntityResolver;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import com.sun.jbi.common.descriptor.parsers.DefaultCatalogResolver;
import com.sun.jbi.common.util.I18n;
import com.sun.jbi.common.util.Util;

/**
 * 
 * @author Kevan Simpson
 */
public abstract class CatalogResolver implements EntityResolver, URIResolver {
    public static final String CATALOG_NS = "urn:oasis:names:tc:entity:xmlns:xml:catalog";
    
    private static Logger mLogger = Logger.getLogger(CatalogResolver.class.getName());
    
    private String mRootPath;

    protected CatalogResolver() {
    }

    protected CatalogResolver(String rootPath) {
        setRootPath(rootPath);
    }

    /** @see org.xml.sax.EntityResolver#resolveEntity(java.lang.String, java.lang.String) */
    public InputSource resolveEntity(String publicId, String systemId)
            throws SAXException, IOException {
        File file = resolveFile(systemId);
        InputSource src = null;
        
        if (file != null) {
            src = new InputSource(new FileReader(file));
            src.setSystemId(systemId);
            src.setPublicId(publicId);
        }
        
        return src;
    }

    /**
     * Resolves the specified systemId to a valid URI string.
     * @param systemId The specified systemId.
     * @return A valid URI or <code>null</code>.
     */
    public abstract String resolve(String systemId);

    /**
     * Resolves the specified systemId to a {@link File}.
     * @param systemId A system id.
     * @return A <code>File</code> or <code>null</code>.
     */
    public File resolveFile(String systemId) {
        URI uri = resolveURI(systemId);
        return (uri == null) ? null : new File(uri);
    }

    /**
     * Resolves the specified systemId to a {@link URI}.
     * @param systemId A system id.
     * @return A <code>URI</code> or <code>null</code>.
     */
    public URI resolveURI(String systemId) {
        String uri = resolve(systemId);
        try {
            return (Util.isEmpty(uri)) ? null : new URI(uri);
        }
        catch (URISyntaxException use) {
            log().log(Level.FINE, I18n.format(
                    "UTIL-3003: Unable to parse or resolve URI: {0}", 
                    String.valueOf(uri)), use);
            return null;
        }
    }
    
    /** 
     * Returns the rootPath.
     * @return the rootPath. 
     */
    protected String getRootPath() {
        return mRootPath;
    }

    /**
     * Sets the rootPath. 
     * @param rootPath The rootPath to set. */
    protected void setRootPath(String rootPath) {
        mRootPath = rootPath;
    }

    protected Logger log() {
        return mLogger;
    }
    
    /**
     * Creates a new instance of a {@link CatalogResolver} using the specified
     * path to locate catalog files.
     * 
     * @param rootPath The base path of the returned <code>CatalogResolver</code>.
     * @return A <code>CatalogResolver</code>.
     * @throws DeploymentException if an error occurs initializing the resolver.
     */
    public static CatalogResolver newInstance(String rootPath) throws DeploymentException {
        return new DefaultCatalogResolver(rootPath);
    }

    /**
     * Logs specified {@link Exception} and message, then wraps and throws both
     * in a {@link DeploymentException}.
     * 
     * @param thrown The error to log and rethrow.
     * @param msg The message to log.
     * @return A <code>DeploymentException</code>.
     */
    protected static DeploymentException error(Exception thrown, String msg) {
        if (thrown == null) {
            mLogger.warning(msg);
        }
        else {
            mLogger.log(Level.WARNING, msg, thrown);
        }
        
        if (thrown instanceof DeploymentException) {
            return (DeploymentException) thrown;
        }
        else if (thrown != null) {
            return new DeploymentException(msg, thrown);
        }
        
        return new DeploymentException(msg);
    }
}
