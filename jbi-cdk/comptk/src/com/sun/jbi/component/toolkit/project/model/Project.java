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
 * @(#)Project.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.project.model;

import java.io.File;
import java.io.FileFilter;
import java.util.HashMap;
import java.util.Map;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import com.sun.jbi.common.util.Util;
import com.sun.jbi.component.toolkit.project.CDK;
import com.sun.jbi.component.toolkit.project.model.Expr.ProjectExpr;
import com.sun.jbi.component.toolkit.project.model.Pom.Module;
import com.sun.jbi.component.toolkit.project.util.XPathElement;

/**
 * 
 * @author Kevan Simpson
 */
public class Project extends XPathElement {// XmlObject<ProjectExpr> {
    private JbiComponent mComponent;
    private ServiceConfig mSrvcConfig;
    private Map<String, Pom> /*mPomMap,*/ mModules;
//    private String mVersion;
    private File mProjRoot, mAsAdmin, mInstallerJar;
    
    public Project(Element elem, File file) {   // new project
        super(file, elem);
        init();
//        super(elem, file, ProjectExpr.values());
    }

    protected void init() {
        mProjRoot = getFile().getParentFile();
        String asAdmin = getString(ProjectExpr.as_script.getXPath());
        if (Util.isEmpty(asAdmin)) {
            // try and guess
            String gf = System.getProperty(CDK.GF_HOME);
            if (!Util.isEmpty(gf)) {
                mAsAdmin = new File(new File(new File(gf), "bin"), "asadmin.bat");
                if (!mAsAdmin.exists()) {
                    mAsAdmin = null;
                }
            }
            
            if (mAsAdmin == null) {
                mAsAdmin = new File(mProjRoot, "dummy-asadmin.bat");
            }
        }
        else {
            mAsAdmin = new File(asAdmin);
        }
        // initialize component
        File jbi = new File(
                getString(ProjectExpr.component_descriptor.getXPath()));
        mComponent = new JbiComponent(XPathElement.loadXmlFile(jbi));
        // initialize services
        mSrvcConfig = new ServiceConfig((Element) 
                getNode(ProjectExpr.services.getXPath()), getFile());
        // initialize pom map
        NodeList poms = getNodeSet(ProjectExpr.poms.getXPath());
//        mPomMap = new HashMap<String, Pom>();
        mModules = new HashMap<String, Pom>();
        for (int i = 0, n = poms.getLength(); i < n; i++) {
            Element pomElem = (Element) poms.item(i);
            File file = new File(pomElem.getAttribute("file"));
            String nm = pomElem.getAttribute("name");
            Pom mod = new Pom(XPathElement.loadXmlFile(file));
            mModules.put(nm, mod);
        }
        
        // look for version property
//        Pom versions = Pom.loadGlobalPom(mProjRoot.getParentFile());
//        if (versions != null) {
//            Element cfg = (Element) 
//                    versions.getValue(PomExpr.gc_properties);
//            String v = resolveProperty(cfg, getName() +".artifact.version");
//            if (v == null) {
//                v = resolveProperty(cfg, "jbicomps.currentVersion");
//            }
//            mVersion = v; 
//        }
    }
    
    protected String resolveProperty(Element cfg, String key) {
        NodeList list = cfg.getElementsByTagName(key);
        if (list != null && list.getLength() == 1) {
            Element prop = (Element) list.item(0);
            String value = prop.getTextContent();
            if (value.startsWith("${")) {
                return resolveProperty(cfg, value.substring(2, value.indexOf("}")));
            }
            else {
                return value;
            }
        }
        
        return null;
    }

    public void addModule(String name, Pom pom) {
        if (!Util.isEmpty(name) && pom != null) {
            mModules.put(name, pom);
        }
    }

    /** 
     * Returns the asAdmin.
     * @return the asAdmin. 
     */
    public File getAsAdmin() {
        return mAsAdmin;
    }

    public JbiComponent getComponent() {
        return mComponent;
    }
    
    public File getInstallerJar() {
        if (mInstallerJar == null) {
            File bld = new File(new File(
                    getFile().getParentFile(), Module.packaging.toString()), "bld");
            if (bld.exists()) {
                File[] files = bld.listFiles(new FileFilter() {
                    public boolean accept(File f) {
                        return (f.exists() && f.getName().startsWith(getName()));
                    }
                });
                if (files != null && files.length == 1) {
                    mInstallerJar = files[0];
                }
            }
        }
        
        return mInstallerJar;
    }
    
    public Pom getModule(String name) {
        return mModules.get(name);
    }

    public String[] getModuleNames() {
        String[] nms = new String[mModules.size()];
        mModules.keySet().toArray(nms);
        return nms;
    }
    
    public String getName() {
        return getRoot().getName();
    }
    
    public File getRoot() {
        return mProjRoot;
    }
    
    public ServiceConfig getServiceConfig() {
        return mSrvcConfig;
    }
    
//    public String getVersion() {
//        return mVersion;
//    }
    
    public Pom removeModule(String name) {
        return mModules.remove(name);
    }

    /**
     * Sets the asAdmin. 
     * @param asAdmin The asAdmin to set. */
    public void setAsAdmin(File asAdmin) {
        mAsAdmin = asAdmin;
        String path = asAdmin.getAbsolutePath();
        setValue(ProjectExpr.as_script.getXPath(), path);
        System.setProperty(CDK.GF_HOME, path);
    }
}
