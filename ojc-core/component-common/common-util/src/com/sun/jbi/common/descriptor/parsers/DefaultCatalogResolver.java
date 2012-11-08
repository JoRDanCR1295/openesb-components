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
 * @(#)DefaultCatalogResolver.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.descriptor.parsers;

import java.io.File;
import java.util.Vector;
import java.util.logging.Level;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.jbi.management.DeploymentException;
import javax.xml.transform.Source;
import javax.xml.transform.TransformerException;
import org.apache.xml.resolver.Catalog;
import org.apache.xml.resolver.CatalogEntry;
import org.apache.xml.resolver.CatalogException;
import org.apache.xml.resolver.CatalogManager;
import com.sun.jbi.common.descriptor.CatalogResolver;
import com.sun.jbi.common.util.I18n;
import com.sun.jbi.common.util.Util;

/**
 * 
 * @author Kevan Simpson
 */
public class DefaultCatalogResolver extends CatalogResolver {
    private CatalogManager mCatalogMgr;
    private org.apache.xml.resolver.tools.CatalogResolver mResolver;
    private boolean mWindows = false;  // part of hack to mimic Apache patch
    
    /**
     * 
     */
    public DefaultCatalogResolver(String rootPath) throws DeploymentException {
        super(rootPath);
        init();
    }
    
    protected void init() throws DeploymentException {
        if (Util.isEmpty(getRootPath())) return;
        
        // set up the entity resolver here
        File catalog = new File(getRootPath() + File.separator + "META-INF" 
                + File.separator + "catalog.xml");
        mCatalogMgr = new CatalogManager();
        mCatalogMgr.setIgnoreMissingProperties(true);
        if (catalog.exists()) {
            mCatalogMgr.setCatalogFiles(catalog.getAbsolutePath());
        }
        else {
            log().warning(I18n.loc(
                    "UTIL-6015: Catalog file not found at {0}!", 
                    catalog.getAbsolutePath()));
        }
        mCatalogMgr.setRelativeCatalogs(true);
        mCatalogMgr.setUseStaticCatalog(false);
        mResolver = new org.apache.xml.resolver.tools.CatalogResolver(mCatalogMgr);
        
        // This is a hack to mimic the patch available for Apache Bug 45207
        // https://issues.apache.org/bugzilla/show_bug.cgi?id=45207
        String osname = System.getProperty("os.name");
        mWindows = (osname.indexOf("Windows") >= 0);
        // verify BASE is not screwed up too...
        String base = mResolver.getCatalog().getCurrentBase();
        if (needsFixing(base)) {
            String newBase = fixEntity(base, "BASE");
            Vector<String> args = new Vector<String>();
            args.add(newBase);
            try {
                mResolver.getCatalog().addEntry(new CatalogEntry(Catalog.BASE, args));
            }
            catch (CatalogException ce) {
                throw error(ce, I18n.loc(
                        "UTIL-6016: Failed to resolve catalog base {0} (original base = {1})", 
                        newBase, base));
            }
        }
        
        if (log().isLoggable(Level.FINE)) {
            log().fine(I18n.format(
                    "UTIL-3002: Initialized {0} successfully with base: {1}",
                    this.getClass().getSimpleName(), 
                    mResolver.getCatalog().getCurrentBase()));
        }
    }

    /** @see com.sun.jbi.common.descriptor.CatalogResolver#resolve(java.lang.String) */
    @Override
    public String resolve(String systemId) {
        String entity = mResolver.getResolvedEntity(null, systemId);
        return mWindows ? fixEntity(entity, systemId) : entity;
    }

    private boolean needsFixing(String entity) {
        if (mWindows && !Util.isEmpty(entity) && entity.startsWith("file:")) {
            // This is a hack to mimic the patch available for Apache Bug 45207
            // https://issues.apache.org/bugzilla/show_bug.cgi?id=45207
            Matcher m = Pattern.compile("^file:[^/]").matcher(entity);
            return m.find();
        }
        
        return false;
    }
    
    private String fixEntity(String entity, String systemId) {
        if (Util.isEmpty(entity)) return entity;
        
        // This is a hack to mimic the patch available for Apache Bug 45207
        // https://issues.apache.org/bugzilla/show_bug.cgi?id=45207
        
        // This method should ONLY be called if needsFixing() returns true...
        String newEntity = "file:/" + entity.substring(5);
        if (log().isLoggable(Level.FINER)) {
            log().finer(I18n.format(
                    "UTIL-2002: Modified resolved entity for systemId - {0} - on Windows from {1} to {2}",
                    systemId, entity, newEntity)); 
        }

        return newEntity;
    }
    
    /** @see javax.xml.transform.URIResolver#resolve(java.lang.String, java.lang.String) */
    public Source resolve(String href, String base) throws TransformerException {
        return mResolver.resolve(href, base);
    }
}
