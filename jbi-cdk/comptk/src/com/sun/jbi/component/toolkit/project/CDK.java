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

package com.sun.jbi.component.toolkit.project;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import com.sun.jbi.common.util.Util;
import com.sun.jbi.common.xml.XmlUtil;
import com.sun.jbi.component.toolkit.project.GenerateTokens.Tkn;
import com.sun.jbi.component.toolkit.project.GenerateTokens.TokenGen;
import com.sun.jbi.component.toolkit.project.model.Pom;
import com.sun.jbi.component.toolkit.project.model.Project;
import com.sun.jbi.component.toolkit.project.model.Expr.PomExpr;
import com.sun.jbi.component.toolkit.project.model.Expr.ProjectExpr;
import com.sun.jbi.component.toolkit.project.model.Pom.Module;
import com.sun.jbi.component.toolkit.project.scrub.Profile.ProjType;
import com.sun.jbi.component.toolkit.project.util.I18n;
import com.sun.jbi.component.toolkit.project.util.ProjectException;
import com.sun.jbi.component.toolkit.project.util.XPathElement;
import com.sun.jbi.component.toolkit.project.util.XmlWriter;
import com.sun.jbi.component.toolkit.project.util.XPathElement.NS;
import com.sun.jbi.component.toolkit.project.view.MainView;
import com.sun.jbi.component.toolkit.project.view.wizard.CreateWizard;

/**
 * 
 * @author Kevan Simpson
 */
public class CDK {
    public enum Axn { create, open, wizard }
    
    public static final String OJC_ROOT = "JV_SRCROOT";
    public static final String GF_HOME  = "JV_GFBASE";
    public static final String CDK_HOME = "CDK_HOME";
    public static final String ANT_HOME = "ant.home";
    
    public static void main(String[] args) {
        try {
            /*
            <arg value="${axn}"/>
            <arg path="${env.JV_SRCROOT}"/>
            <arg path="${env.JV_GFBASE}"/>
            <arg path="${basedir}"/>
             */
            if (args == null || args.length < 4) {
                System.out.println("CDK Error: Invalid arguments - expects:\n"+
                        "\t1) (open|create|wizard)\n"+
                        "\t2) $ant.home\n"+
                        "\t3) ($"+ OJC_ROOT +")?\n"+
                        "\t4) ($"+ GF_HOME +")?\n"+
                        "\t5) $"+ CDK_HOME +"\n"+
                        "\t6) -Dproject.name=<path-to-jbic-or-pom>");
                System.exit(1);
            }
            
            Axn axn = Axn.valueOf(args[0]);
            // make available to AsAdmin-related Process instances
            String antHome = argToSysProp(args[1], ANT_HOME, true);
            String ojcRoot = argToSysProp(args[2], OJC_ROOT, false);
            String gfBase = argToSysProp(args[3], GF_HOME, false);
            String cdkHome = argToSysProp(args[4], CDK_HOME, true);
            String name = args.length > 5 ? args[5] : null;
            
            File jbicFile = null;
            Project proj = null;
            switch (axn) {
                // Axn.wizard --> creates wizard.properties
                case wizard: {
                    runWizard();    // creates tokens only
                    break;
                }
                // Axn.create --> creates .jbic file
                case create: {
                    if (name != null) {
                        name = (new File(name)).getName();
                    }
                    File cdkBase = new File(cdkHome);
                    Properties prop = new Properties();
                    prop.load(new FileInputStream(new File(
                            new File(cdkBase, "bld"), name +".properties")));
//                    System.out.println(prop);
                    // determine JV_SRCROOT, if available/needed
                    String projRoot = prop.getProperty(Tkn.PROJ_ROOT.toKey());
                    if (ojcRoot.contains("${env."+ OJC_ROOT +"}")) {  // unset
                        ProjType projType = ProjType.valueOf(
                                prop.getProperty(Tkn.PROFILE.toKey()));
                        switch (projType) {
                            case ojc: { // projRoot's parent is ojc-core
                                ojcRoot = (new File(projRoot)).getParentFile()
                                        .getParentFile().getAbsolutePath();
                                break;
                            }
                            case contrib: { // projRoot's parent is ojc
                                ojcRoot = (new File(projRoot))
                                        .getParentFile().getAbsolutePath();
                                break;
                            }
                        }
                        System.out.println(projType +"-"+ ojcRoot);
                    }
                    if (!ojcRoot.contains("${env."+ OJC_ROOT +"}")) {  // now set
                        System.setProperty(OJC_ROOT, ojcRoot);
                    }
                    
                    // create and load project
                    jbicFile = new File(projRoot, name +".jbic");
                    System.out.println(jbicFile.getAbsolutePath());
                    createProject(jbicFile);
                    proj = loadProject(jbicFile);
                    break;
                }
                // Axn.open   --> opens editor
                case open: {
                    System.out.println("OPEN: "+ name);
                    if (!Util.isEmpty(name) && !name.contains("${project.name}")) {
                        // user has specified path to jbic file...maybe
                        jbicFile = jbicFromPom(new File(name));
                    }
                    else {
                        jbicFile = openJbicFile();
                    }
                    
                    if (jbicFile == null) {
                        System.out.println("CDK: No JBIC file selected... exiting CDK.");
                        System.exit(1);
                    }
                    else if (!jbicFile.exists() || 
                             jbicFile.getName().equals("pom.xml")) {
                        System.out.println("Creating JBIC file for "+ 
                                jbicFile.getParentFile().getName());
                        createProject(jbicFile);
                    }
                    proj = loadProject(jbicFile);
                }
            }

            if (proj != null) {
                System.out.println("Loading "+ jbicFile.getAbsolutePath());
                MainView view = new MainView(proj);
                view.setVisible(true);
            }
            else {  // Ant target executing this app will hang otherwise...
                System.exit(0);
            }
        }
        catch (Exception e) {
            System.err.println(I18n.loc(    // XXX
                    "CDK Error: {0}", e.getMessage()));
            e.printStackTrace();
            System.exit(1); // do NOT continue project creation
        }
    }

    public static String argToSysProp(String arg, String sysProp, boolean req) {
        if (!Util.isEmpty(arg) && !arg.contains("${")) {
            System.setProperty(sysProp, arg);
        }
        else if (req) {
            throw new ProjectException("" +
            		"CDK Error: Missing required environment variable - "+ sysProp);
        }
        
        return arg;
    }
    
    private static File openJbicFile() {
        // query user
        JFileChooser jfc = new JFileChooser(
                new File(System.getProperty("CDK_HOME")));
        jfc.setApproveButtonText("Open");
        jfc.setApproveButtonMnemonic('O');
        jfc.setFileSelectionMode(JFileChooser.FILES_ONLY);
        jfc.setDialogTitle("Open JBI Component Project...");
        
        // show dialog until cancelled or user selects .jbic or top-level POM
        int opt = jfc.showOpenDialog(null);
        File jbic = null;
        while (opt == JFileChooser.APPROVE_OPTION) {
            jbic = jfc.getSelectedFile();
            if (jbic == null || 
                (!jbic.getAbsolutePath().endsWith(".jbic") && 
                 !jbic.getAbsolutePath().endsWith("pom.xml"))) {
                // instruct user to select .jbic or top-level POM
                JOptionPane.showMessageDialog(null, 
                        "Please select a JBIC file or a component project's top-level POM!", 
                        "Invalid JBIC File Selection!", 
                        JOptionPane.WARNING_MESSAGE);
            }
            else {
                System.out.println("OPEN: "+ jbic);
                break;
            }
            
            opt = jfc.showOpenDialog(null);
        }
        
        return jbicFromPom(jbic);
    }
    
    private static File jbicFromPom(File jbic) {
        if (jbic != null && jbic.exists()) {
            if (jbic.isDirectory()) {
                File cdk = new File(jbic, jbic.getName() +".jbic");
                if (cdk.exists()) {
                    return cdk;
                }
                else {
                    File pom = new File(jbic, "pom.xml");
                    if (pom.exists()) {
                        jbic = pom;
                    }
                }
            }
            
            if (jbic.getName().equals("pom.xml")) {
                // create the .jbic file
                File parent = jbic.getParentFile();
                jbic = new File(parent, parent.getName() +".jbic");
            }
        }
        
        return jbic;
    }
    
    private static File runWizard() {
        System.out.println("Running wizard...");
        
        File cdkHome = new File(System.getProperty("user.dir"));
//        System.out.println(cdkHome.getAbsolutePath());
//        System.out.println(typeDesc +" - "+ projName);
        try {
            System.out.println("creating TokenGen...");
            TokenGen tknGen = new TokenGen(/*null, 
                                           new File(projRootParentDir, projName),*/ 
                                           new File(cdkHome, "wizard.properties")/*,
                                           new File(new File(cdkHome, "templates"),
                                                    "scrub-config.xml"), 
                                           Scrubber.DEFAULT_PROFILE*/);
            Map<Tkn, String> tokens = null;
            if (true) { // in development...
                System.out.println("Showing wizard...");
                CreateWizard wiz = new CreateWizard(tknGen);
                wiz.setVisible(true);
                if (!wiz.isCanceled()) {
                    tokens = wiz.getTokens();//tknGen.getDefaultTokens();
                    System.out.println("Wizard Tokens:");
                    for (Tkn tkn : tokens.keySet()) {
                        System.out.println(tkn +" = "+ tokens.get(tkn));
                    }
//                    tokens = null;  // testing...
                }
            }
            else {  // working...
                tknGen.initDefaultTokens();
                tokens = tknGen.getTokens();
            }
            
            if (tokens != null) {
                tknGen.writeTokens(tokens);
            }
            else {
                System.exit(1); // user cancelled
            }
        }
        catch (IOException ioe) {
            ioe.printStackTrace();
            throw new ProjectException(I18n.loc(
                    "JBIC Wizard failed: {0}", ioe.getMessage()), ioe);
        }
        // TODO show dialog to acquire user input
        // TODO create TokenGen and write tokens
        // TODO determine if we invoke Ant task to copy templates or do it programmatically
        // TODO scrub project files
        // TODO create .jbic file and return
        File jbic = null;
        return jbic;
    }
    
    /**
     * Creates a new JBIC project file, with the assumption that the project
     * has already been created (likely using the CDK distribution).
     * 
     * @param file The JBIC project file, which need not exist prior to calling this method.
     */
    public static void createProject(File file) {
        if (file == null) return;

        File jbic = null;
        if (file.exists() && file.getName().equals("pom.xml")) {
            jbic = new File(file.getParentFile(), 
                            file.getParentFile().getName() +".jbic");
        }
        else {
            jbic = file;
        }

        XPathElement xelem = null;
        if (jbic.exists()) {
            System.out.println(
                    "CDK is overwriting JBIC file at: "
                        + jbic.getAbsolutePath());
        }
        
        // create new JBIC file
        try {
            String gfProp = System.getProperty(GF_HOME);
            File asRoot = Util.isEmpty(gfProp) ? null : new File(gfProp);
            File template = new File(CDK.class.getResource("jbic.xml").toURI());
            xelem = XPathElement.loadXmlFile(template);
            // cheat and look for GF asadmin.bat
            if (asRoot != null && asRoot.exists()) {
                File script = new File(new File(asRoot, "bin"), "asadmin.bat");
                if (script.exists()) {
                    xelem.setValue(ProjectExpr.as_script.getXPath(), 
                                   script.getAbsolutePath());
                }
            }

            File projRoot = jbic.getParentFile();
            xelem.setValue(ProjectExpr.component_root.getXPath(), 
                           projRoot.getAbsolutePath());
            // look for JBI component descriptor in packaging module
            File desc = new File(new File(new File(
                    projRoot, Module.packaging.toString()), "src"), "jbi.xml");
            if (desc.exists()) {
                xelem.setValue(ProjectExpr.component_descriptor.getXPath(), 
                               desc.getAbsolutePath());
            }
            // TODO what happens if no JBI descriptor?
            
            // then find all the poms
            File tpFile = new File(projRoot, "pom.xml");
            if (tpFile.exists()) {  // top-level pom is required to find modules
                Map<String, String> poms = new HashMap<String, String>();
                Pom topPom = new Pom(XPathElement.loadXmlFile(tpFile));
                poms.put(Module.top_level.toString(), tpFile.getAbsolutePath());
                
                NodeList mods = topPom.getNodeSet(PomExpr.modules.getXPath());
                if (mods != null) {
                    for (int i = 0, n = mods.getLength(); i < n; i++) {
                        String name = mods.item(i).getTextContent();
                        File modPomFile = new File(
                                new File(projRoot, name), "pom.xml");
                        if (modPomFile.exists()) {
                            poms.put(name, modPomFile.getAbsolutePath());
                        }
                    }
                }
             
                Element mvn = (Element) 
                        xelem.getNode(ProjectExpr.maven.getXPath());
                Document doc = xelem.getElement().getOwnerDocument();
                for (String name : poms.keySet()) {
                    Element pom = doc.createElementNS(NS.CDK.getUri(), "pom");
                    pom.setAttribute("name", name);
                    pom.setAttribute("file", poms.get(name));
                    mvn.appendChild(pom);
                }
            }

            // write to file
            XmlWriter xw = new XmlWriter(false);
            xw.writeFile(xelem.getElement(), jbic);
        }
        catch (ProjectException pe) {
            throw pe;   // already logged
        }
        catch (Exception e) {
            // XXX
            e.printStackTrace();
            throw new ProjectException(I18n.loc(    // XXX
                    "Failed to create project {0}: {1}",
                    file.getName(), e.getMessage()));
        }
    }
    
    public static Project loadProject(File file) {
        try {
            Document doc = XmlUtil.readXml(file);
            if (doc != null) {
                Element elem = doc.getDocumentElement();
                Project proj = new Project(elem, file);
                return proj;
            }
        }
        catch (ProjectException pe) {
            throw pe;   // already logged
        }
        catch (Exception e) {
            e.printStackTrace();
            throw new ProjectException(I18n.loc(    // XXX
                    "Failed to load project from {0}: {1}",
                    String.valueOf(file), e.getMessage()));
        }
        
        return null;
    }
    
    /*
     *  
            <arg value="${axn}"/>
            <arg path="${env.JV_SRCROOT}"/>
            <arg path="${env.JV_GFBASE}"/>
            <arg path="${env.MAVEN_HOME}"/>
            <arg value="${project.name}"/>

     *  // TODO move this to wiki
     *  CDK Use Cases:
     *      1.  create
     *          1.1.    run wizard
     *              1.1.1.  panel: .jbic data
     *              1.1.2.  panel: jbi component data
     *              1.1.3.  panel: pom data (integrate into ojc-core?, et al.)
     *          1.2.    wizard creates token file
     *          1.3.    ant verifies existence of token file
     *          1.4.    ant creates project using token file
     *          
     *      2.  open
     *          2.1.    if 'project.name' is specified:
     *              2.1.1.  look for .jbic in ojc-core/${project.name}
     *              2.1.2.  if exists, goto 2.3
     *              2.1.3.  if not exists:
     *                  2.1.3.1.    create .jbic file (user types file name)
     *                  2.1.3.2.    goto 2.3
     *          2.2.    file dialog, root=ojc-core, *.jbic
     *          2.3.    create Project from .jbic file
     *          2.4.    load Project in editor
     *          
     */
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
}
