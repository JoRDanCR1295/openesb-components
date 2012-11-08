/*
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License (the License). You may not use this file except in
 * compliance with the License.
 *
 * You can obtain a copy of the License at http://www.netbeans.org/cddl.html
 * or http://www.netbeans.org/cddl.txt.
 *
 * When distributing Covered Code, include this CDDL Header Notice in each file
 * and include the License file at http://www.netbeans.org/cddl.txt.
 * If applicable, add the following below the CDDL Header, with the fields
 * enclosed by brackets [] replaced by your own identifying information:
 * "Portions Copyrighted [year] [name of copyright owner]"
 *
 * The Original Software is NetBeans. The Initial Developer of the Original
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2006 Sun
 * Microsystems, Inc. All Rights Reserved.
 */

package org.netbeans.modules.jbi.apisupport;

import java.awt.Image;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.netbeans.modules.xml.catalog.spi.CatalogDescriptor;
import org.netbeans.modules.xml.catalog.spi.CatalogListener;
import org.netbeans.modules.xml.catalog.spi.CatalogReader;
import org.openide.util.NbBundle;
import org.openide.util.Utilities;

/** Catalog for web app DTDs that enables completion support in editor.
 *
 * @author chikkala
 */
public class JbiXmlCatalog implements CatalogReader, CatalogDescriptor, org.xml.sax.EntityResolver {
    
    public static final String JBI_NS = "http://java.sun.com/xml/ns/jbi"; // NOI18N
    public static final String JBI_NS_XSD = JBI_NS + "/jbi.xsd"; // NOI18N
    public static final String JBI_NS_XSD_1 = "./jbi.xsd"; // NOI18N
    public static final String JBI_NS_XSD_2 = "jbi.xsd"; // NOI18N
    public static final String JBI_NS_URL = "nbres:/org/netbeans/modules/jbi/apisupport/resources/schema/jbi-descriptor.xsd"; // NOI18N
    
    public static final String JBI_MGMT_MSG_NS = "http://java.sun.com/xml/ns/jbi/management-message"; // NOI18N
    public static final String JBI_MGMT_MSG_NS_XSD = JBI_MGMT_MSG_NS + "/management-message.xsd"; // NOI18N
    public static final String JBI_MGMT_MSG_NS_XSD_1 = "./management-message.xsd"; // NOI18N
    public static final String JBI_MGMT_MSG_NS_XSD_2 = "management-message.xsd"; // NOI18N
    public static final String JBI_MGMT_MSG_NS_URL = "nbres:/org/netbeans/modules/jbi/apisupport/resources/schema/jbi-management-message.xsd"; // NOI18N
    
    private Map mIdMap;
    private List mSystemIdList;
    
    /** Creates a new instance of DDCatalog */
    public JbiXmlCatalog() {
        initCatalog();
    }
    
    private void initCatalog() {
        
        this.mIdMap = new HashMap();
        this.mSystemIdList = new ArrayList();
        
        this.mSystemIdList.add(JBI_NS_URL);
        this.mSystemIdList.add(JBI_MGMT_MSG_NS_URL);
        
        this.mIdMap.put(JBI_NS, JBI_NS_URL);
        this.mIdMap.put(JBI_NS_XSD, JBI_NS_URL);
        this.mIdMap.put(JBI_NS_XSD_1, JBI_NS_URL);
        this.mIdMap.put(JBI_NS_XSD_2, JBI_NS_URL);
        
        this.mIdMap.put(JBI_MGMT_MSG_NS, JBI_MGMT_MSG_NS_URL);
        this.mIdMap.put(JBI_MGMT_MSG_NS_XSD, JBI_MGMT_MSG_NS_URL);
        this.mIdMap.put(JBI_MGMT_MSG_NS_XSD_1, JBI_MGMT_MSG_NS_URL);
        this.mIdMap.put(JBI_MGMT_MSG_NS_XSD_2, JBI_MGMT_MSG_NS_URL);
        
        
    }
    /**
     * Get String iterator representing all public IDs registered in catalog.
     * @return null if cannot proceed, try later.
     */
    public java.util.Iterator getPublicIDs() {
        Set keySet = this.mIdMap.keySet();
        return Collections.unmodifiableSet(keySet).iterator();
    }
    
    /**
     * Get registered systemid for given public Id or null if not registered.
     * @return null if not registered
     */
    public String getSystemID(String publicId) {
        // Utils.debug("JbiXmlCatalog:getSystemID: publicId = " + publicId);
        
        return (String) this.mIdMap.get(publicId);
    }
    
    /**
     * Refresh content according to content of mounted catalog.
     */
    public void refresh() {
    }
    
    /**
     * Optional operation allowing to listen at catalog for changes.
     * @throws UnsupportedOpertaionException if not supported by the implementation.
     */
    public void addCatalogListener(CatalogListener l) {
    }
    
    /**
     * Optional operation couled with addCatalogListener.
     * @throws UnsupportedOpertaionException if not supported by the implementation.
     */
    public void removeCatalogListener(CatalogListener l) {
    }
    
    /** Registers new listener.  */
    public void addPropertyChangeListener(java.beans.PropertyChangeListener l) {
    }
    
    /**
     * @return I18N display name
     */
    public String getDisplayName() {
        return NbBundle.getMessage(JbiXmlCatalog.class, "LBL_JbiXmlCatalog");
    }
    
    /**
     * Return visuaized state of given catalog.
     * @param type of icon defined by JavaBeans specs
     * @return icon representing current state or null
     */
    public Image getIcon(int type) {
        Image catImg = Utilities.loadImage("org/netbeans/modules/jbi/apisupport/resources/images/xmlCatalog.gif"); // NOI18N
        Image jbiImg = Utilities.loadImage("org/netbeans/modules/jbi/apisupport/resources/images/JBI.png"); // NOI18N
        return Utilities.mergeImages(catImg, jbiImg, 8,8);
        
    }
    
    /**
     * @return I18N short description
     */
    public String getShortDescription() {
        return NbBundle.getMessage(JbiXmlCatalog.class, "DESC_JbiXmlCatalog");
    }
    
    /** Unregister the listener.  */
    public void removePropertyChangeListener(java.beans.PropertyChangeListener l) {
    }
    
    /**
     * Resolves schema definition file
     * @param publicId publicId for resolved entity
     * @param systemId systemId for resolved entity
     * @return InputSource for
     */
    public org.xml.sax.InputSource resolveEntity(String publicId, String systemId) throws org.xml.sax.SAXException, java.io.IOException {
        //  Utils.debug("JbiXmlCatalog:resolveEntity: publicId= " + publicId + "\n SystemID= " + systemId);
        String url = null;
        
        if (  publicId != null  ) {
            url = (String) this.mIdMap.get(publicId);
        } else if ( systemId != null ) {
            url = (String) this.mIdMap.get(systemId);
            if ( systemId.endsWith("jbi.xsd") ) {
                url = (String) this.mIdMap.get(JBI_NS);
            }
        }
        
        if ( url != null ) {
            return new org.xml.sax.InputSource(url);
        } else {
            return null;
        }
        
    }
    
    /**
     * Get registered URI for the given name or null if not registered.
     * @return null if not registered
     */
    public String resolveURI(String name) {
//        Utils.debug("JbiXmlCatalog: resolveURI = " + name);
        // return null;
        return (String) this.mIdMap.get(name);
    }
    /**
     * Get registered URI for the given publicId or null if not registered.
     * @return null if not registered
     */
    public String resolvePublic(String publicId) {
//        Utils.debug("JbiXmlCatalog:resolvePublic: publicId = " + publicId);
        return (String) this.mIdMap.get(publicId);
        // return null;
    }
    
}
