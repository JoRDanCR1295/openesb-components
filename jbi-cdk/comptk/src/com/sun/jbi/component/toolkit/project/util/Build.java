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
 * @(#)Build.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.project.util;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import javax.xml.namespace.QName;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.descriptor.JbiDescriptor;
import com.sun.jbi.common.descriptor.model.Services;
import com.sun.jbi.common.util.Util;
import com.sun.jbi.component.toolkit.project.CDK;
import com.sun.jbi.component.toolkit.project.model.Project;
import com.sun.jbi.component.toolkit.project.model.ServiceConfig;
import com.sun.jbi.component.toolkit.project.services.ServicesBuilder;
import com.sun.jbi.component.toolkit.project.view.App;

/**
 * Helps Ant script build installable JBI component.
 * @author Kevan Simpson
 */
public class Build {
    public enum Ant implements Exec {
        build_installer,
        service_unit;
        
        /** @see com.sun.jbi.component.toolkit.project.util.Exec#execute(com.sun.jbi.component.toolkit.project.model.Project) */
        public String[] execute(Project proj, String... args) {
            if (proj != null) {
                List<String> list = new ArrayList<String>();
                list.add(getBuild().getProperty(CDK.ANT_HOME)+ "/bin/ant.bat");
//              list.add("-debug");
                File bld = new File(proj.getRoot(), "build.xml");
                list.add("-f");
                list.add(bld.getAbsolutePath());

                list.add(this.toString());

                switch (this) {
                    case service_unit: {
                        /*
                        <property name="su.root" location="${arg.root}"/>
                        <property name="su.desc" location="${arg.desc}"/>
                        <property name="su.temp.jar" value="${arg.temp}"/>
                        <property name="su.jar" value="${arg.jar}"/>
                        */
                        list.add("-Darg.root="+ args[0]);
                        list.add("-Darg.desc="+ args[1]);
                        list.add("-Darg.temp="+ args[2]);
                        list.add("-Darg.jar="+ args[3]);
                        break;
                    }
                    default: // no extra args
                        break;
                }
                String[] params = new String[list.size()];
                list.toArray(params);
                return (new Cmd(this.toString())).execute(proj, params);
            }            
            
            return new String[0];
        }

        /** @see java.lang.Enum#toString() */
        @Override
        public String toString() {
            return super.toString().replace('_', '-');
        }
    }
    
    public static final String CUSTOM_COMPILE_PROP = "custom.compile.order";
    private static Build mSingleton = null;
    
    private File mCdkHome, mFile;
    private Properties mProps;
    
    private Build(File cdkHome) {
        mCdkHome = cdkHome;
        mFile = new File(mCdkHome, "build.properties");
        mProps = new Properties();
        try {
            mProps.load(new FileInputStream(mFile));
        }
        catch (Exception e) {
            // TODO throw this?
            throw new ProjectException("CDK Error: Failed to load build: "+ e.getMessage(), e);
        }
    }

    public String buildServicesDescriptor(Project proj, String serviceDef,
                                          String unitName) {
        try {
            // generate services
            ServiceConfig cfg = proj.getServiceConfig();
            ServicesBuilder bldr = cfg.loadBuilder(serviceDef);
            Services defs = bldr.generateServices(
                    unitName, cfg.getUnitLocation(unitName)); 
            // build descriptor element
            Document doc = JbiDescriptor.createJbiDescriptorRoot();
            Element jbi = doc.getDocumentElement();
            // append namespaces
            Map<String, String> nsMap = collectNS(defs);
//            System.out.println("collected NS = "+ nsMap);
            for (String uri : nsMap.keySet()) {
                jbi.setAttribute("xmlns:"+ nsMap.get(uri), uri);
            }
            
            Element srvcs = doc.createElementNS(
                    JbiDescriptor.JBI_NS, JbiDescriptor.SERVICES_ELEM);
            srvcs.setAttribute("binding-component", 
                               String.valueOf(proj.getComponent().isBinding()));
            for (EndpointInfo info : defs.getEndpoints()) {
                Element endpt = doc.createElementNS(
                        JbiDescriptor.JBI_NS, 
                        info.isProvides() ? JbiDescriptor.PROVIDES_ELEM
                                          : JbiDescriptor.CONSUMES_ELEM);
                endpt.setAttribute(JbiDescriptor.INTERFACE_ATTR, 
                                   qualify(info.getInterfaceName(), nsMap));
                endpt.setAttribute(JbiDescriptor.SERVICE_ATTR, 
                                   qualify(info.getServiceName(), nsMap));
                endpt.setAttribute(JbiDescriptor.ENDPOINT_ATTR, 
                                   info.getEndpointName());
                if (info.getLinkType() != null) {
                    endpt.setAttribute(JbiDescriptor.LINK_TYPE_ATTR, 
                                       info.getLinkType().toString());
                }
                srvcs.appendChild(endpt);
            }
            jbi.appendChild(srvcs);

//            System.out.println(XmlUtil.print(new DOMSource(jbi)));
            XmlWriter xw = new XmlWriter(false);
            return xw.write(jbi);
        }
        catch (Exception e) {
            throw new ProjectException(I18n.loc(
                    "Failed to generate SU Descriptor {0} using {1}: {2}",
                    serviceDef, unitName, e.getMessage()), e);
        }
    }
    
    private String qualify(QName qname, Map<String, String> nsMap) {
        return nsMap.get(qname.getNamespaceURI()) +":"+ qname.getLocalPart();
    }
    
    /**
     * @param args
     */
    public String[] generateSUArtifact(App app, String serviceDef, String unitName) {
        try {
            Project proj = app.getProject();
            String desc = buildServicesDescriptor(proj, serviceDef, unitName);
            File tempDesc = File.createTempFile(unitName +"-jbi-", ".xml");
            ServiceConfig cfg = proj.getServiceConfig();
            File src = new File(cfg.getUnitLocation(unitName));
            if (src.exists()) {
                // determine su jar names
                String jar = unitName +".jar",
                       tempJar = (proj.getComponent().isBinding())
                               ? proj.getComponent().getName() +".jar"
                               : "SEDeployment.jar";
                // write descriptor to temp location
                Util.writeFile(tempDesc, desc);
                // ant call - root, desc, temp, jar
                CmdAction ant = new CmdAction(app, Ant.service_unit);
                String[] out = ant.execute(proj, 
                                           src.getAbsolutePath(),
                                           tempDesc.getAbsolutePath(),
                                           tempJar, jar);
                return out;
            }
        }
        catch (Exception e) {
            throw new ProjectException(I18n.loc(    // XXX
                    "Failed to generate SU artifact {0}: {1}", 
                    unitName, e.getMessage()), e);
        }
        return new String[0];
    }
    
    
    protected Map<String, String> collectNS(Services srvcs) {
        Map<String, String> map = new HashMap<String, String>();
        int i = 0;
        for (EndpointInfo info : srvcs.getEndpoints()) {
            i = evaluateEndpt(map, i, info.getInterfaceName());
            i = evaluateEndpt(map, i, info.getServiceName());
        }
        
        return map;
    }
    
    private int evaluateEndpt(Map<String, String> map, int ix, QName qname) {
        if (!map.containsKey(qname.getNamespaceURI())) {
            map.put(qname.getNamespaceURI(), "ns"+ ix);
            return ++ix;
        }
        
        return ix;
    }
    
    /**
     * Fetches the build properties of this project.
     * @return the build properties of this project.
     */
    protected Properties getProperties() {
        return mProps;
    }

    public String getProperty(String key) {
        String val = getProperties().getProperty(key);
        return (Util.isEmpty(val)) ? System.getProperty(key) : val;
    }
    
    public void setProperty(String key, String val) {
        try {
            if (!Util.isEmpty(key) && val != null) {
                getProperties().setProperty(key, val);
                getProperties().store(new FileOutputStream(mFile), 
                        "build properties for Component Development Kit (CDK)");
            }
        }
        catch (Exception e) {
            throw new ProjectException("CDK Error: Failed to set property: "+ e.getMessage(), e);
        }
    }
    
    /**
     * Fetches singleton instance of this project's <code>Build</code>.
     * @return singleton instance of this project's <code>Build</code>.
     */
    public static Build getBuild() {
        if (mSingleton == null) {
            mSingleton = new Build(new File(System.getProperty(CDK.CDK_HOME)));
        }
        
        return mSingleton;
    }

    /**
     * @param args
     */
    public static void main(String[] args) {
        try {
            File projRoot = new File(args[0]);
            CDK.argToSysProp(projRoot.getAbsolutePath(), CDK.CDK_HOME, true);
            CDK.argToSysProp(args[1], CDK.OJC_ROOT, false);
            CDK.argToSysProp(args[2], CDK.GF_HOME, false);
            File jbic = new File(projRoot, projRoot.getName() +".jbic");
            Project proj = CDK.loadProject(jbic);
//            getBuild().build(proj);
        }
        catch (Exception e) {
            e.printStackTrace();
        }
    }
}
