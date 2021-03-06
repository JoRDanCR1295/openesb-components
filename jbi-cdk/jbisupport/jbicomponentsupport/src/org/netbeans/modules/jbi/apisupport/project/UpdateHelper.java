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

package org.netbeans.modules.jbi.apisupport.project;

import java.io.IOException;
import javax.swing.JButton;
import org.netbeans.modules.jbi.apisupport.project.JbiCompProjectProperties;
import org.netbeans.spi.project.support.ant.GeneratedFilesHelper;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.util.Exceptions;
import org.w3c.dom.Comment;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;
import org.openide.DialogDisplayer;
import org.openide.ErrorManager;
import org.openide.NotifyDescriptor;
import org.openide.util.NbBundle;
import org.openide.util.Mutex;
import org.netbeans.api.project.Project;
import org.netbeans.api.project.ProjectManager;
import org.netbeans.spi.project.AuxiliaryConfiguration;
import org.netbeans.spi.project.support.ant.AntProjectHelper;
import org.netbeans.spi.project.support.ant.EditableProperties;

/**
 * Proxy for the AntProjectHelper which defers the update of the project metadata
 * to explicit user action. Useful to update the project versions
 */
public class UpdateHelper {
    
    private static final boolean TRANSPARENT_UPDATE = true;
    private static final String BUILD_NUMBER = System.getProperty("netbeans.buildnumber"); // NOI18N
    private static final String MINIMUM_ANT_VERSION_ELEMENT = "minimum-ant-version";
    
    private final Project project;
    private final AntProjectHelper helper;
    private final AuxiliaryConfiguration cfg;
    private final GeneratedFilesHelper genFileHelper;
    private final Notifier notifier;
    private boolean alreadyAskedInWriteAccess;
    private Boolean isCurrent;
    private Element cachedElement;
    
    /**
     * Creates new UpdateHelper
     * @param project
     * @param helper AntProjectHelper
     * @param cfg AuxiliaryConfiguration
     * @param genFileHelper GeneratedFilesHelper
     * @param notifier used to ask user about project update
     */
    UpdateHelper(Project project, AntProjectHelper helper, AuxiliaryConfiguration cfg, GeneratedFilesHelper genFileHelper, Notifier notifier) {
        assert project != null && helper != null && cfg != null && genFileHelper != null && notifier != null;
        this.project = project;
        this.helper = helper;
        this.cfg = cfg;
        this.genFileHelper = genFileHelper;
        this.notifier = notifier;
    }
    
    /**
     * Returns the AntProjectHelper.getProperties(), {@link AntProjectHelper#getProperties(String)}
     * @param path a relative URI in the project directory.
     * @return a set of properties
     */
    public EditableProperties getProperties(final String path) {
        //Properties are the same in all versions of the jbicomponet project
        return (EditableProperties) ProjectManager.mutex().readAccess(new Mutex.Action(){
            public Object run() {
                //                if (!isCurrent() && AntProjectHelper.PROJECT_PROPERTIES_PATH.equals(path)) { //Only project properties were changed
                if (AntProjectHelper.PROJECT_PROPERTIES_PATH.equals(path)) { //Only project properties were changed
                    return getUpdatedProjectProperties();
                } else {
                    return helper.getProperties(path);
                }
            }
        });
    }
    
    /**
     * In the case that the project is of current version or the properties are not {@link AntProjectHelper#PROJECT_PROPERTIES_PATH}
     * it calls AntProjectHelper.putProperties(), {@link AntProjectHelper#putProperties(String, EditableProperties)}
     * otherwise it asks user to updata project. If the user agrees with the project update, it does the update and calls
     * AntProjectHelper.putProperties().
     * @param path a relative URI in the project directory.
     * @param props a set of properties
     */
    public void putProperties(final String path, final EditableProperties props) {
        ProjectManager.mutex().writeAccess(
            new Runnable() {
            public void run() {
                if (isCurrent() || !AntProjectHelper.PROJECT_PROPERTIES_PATH.equals(path)) {  //Only project props should cause update
                    helper.putProperties(path,props);
                } else if (canUpdate()) {
                    try {
                        saveUpdate();
                        helper.putProperties(path,props);
                    } catch (IOException ioe) {
                        ErrorManager.getDefault().notify(ioe);
                    }
                }
            }
        });
    }
    
    /**
     * In the case that the project is of current version or shared is false it delegates to
     * AntProjectHelper.getPrimaryConfigurationData(), {@link AntProjectHelper#getPrimaryConfigurationData(boolean)}.
     * Otherwise it creates an in memory update of shared configuration data and returns it.
     * @param shared if true, refers to <code>project.xml</code>, else refers to
     *               <code>private.xml</code>
     * @return the configuration data that is available
     */
    public Element getPrimaryConfigurationData(final boolean shared) {
        return (Element) ProjectManager.mutex().readAccess(new Mutex.Action(){
            public Object run() {
                if (!shared || isCurrent()) { //Only shared props should cause update
                    return helper.getPrimaryConfigurationData(shared);
                } else {
                    return getUpdatedSharedConfigurationData();
                }
            }
        });
    }
    
    /**
     * In the case that the project is of current version or shared is false it calls AntProjectHelper.putPrimaryConfigurationData(),
     * {@link AntProjectHelper#putPrimaryConfigurationData(Element, boolean)}.
     * Otherwise it asks user to update the project. If the user agrees with the project update, it does the update and calls
     * AntProjectHelper.PrimaryConfigurationData().
     * @param element the configuration data
     * @param shared if true, refers to <code>project.xml</code>, else refers to
     * <code>private.xml</code>
     */
    public void putPrimaryConfigurationData(final Element element, final boolean shared) {
        ProjectManager.mutex().writeAccess(new Runnable() {
            public void run() {
                if (!shared || isCurrent()) {
                    helper.putPrimaryConfigurationData(element, shared);
                } else if (canUpdate()) {
                    try {
                        saveUpdate();
                        helper.putPrimaryConfigurationData(element, shared);
                    } catch (IOException ioe) {
                        ErrorManager.getDefault().notify(ioe);
                    }
                }
            }
        });
    }
    
    /**
     * Returns an AntProjectHelper. The helper may not be used for accessing/storing project metadata.
     * For project metadata manipulation the UpdateHelper must be used.
     * @return AntProjectHelper
     */
    public AntProjectHelper getAntProjectHelper() {
        return this.helper;
    }
    
    /**
     * Request an saving of update. If the project is not of current version the user will be asked to update the project.
     * If the user agrees with an update the project is updated.
     * @return true if the metadata are of current version or updated
     */
    public boolean requestSave() throws IOException{
        if (isCurrent()) {
            return true;
        }
        if (!canUpdate()) {
            return false;
        }
        saveUpdate();
        return true;
    }
    
    /**
     * Returns true if the project is of current version.
     * @return true if the project is of current version, otherwise false.
     */
    public synchronized boolean isCurrent() {
        if (this.isCurrent == null) {
            this.isCurrent = Boolean.TRUE;
        }
        return isCurrent.booleanValue();
    }
    
    private boolean canUpdate() {
        if (TRANSPARENT_UPDATE) {
            return true;
        }
        //Ask just once under a single write access
        if (alreadyAskedInWriteAccess) {
            return false;
        } else {
            boolean canUpdate = this.notifier.canUpdate();
            if (!canUpdate) {
                alreadyAskedInWriteAccess = true;
                ProjectManager.mutex().postReadRequest(new Runnable() {
                    public void run() {
                        alreadyAskedInWriteAccess = false;
                    }
                });
            }
            return canUpdate;
        }
    }
    
    private void saveUpdate() throws IOException {
        this.helper.putPrimaryConfigurationData(getUpdatedSharedConfigurationData(),true);
        ProjectManager.getDefault().saveProject(this.project);
        synchronized(this) {
            this.isCurrent = Boolean.TRUE;
        }
    }
    
    private synchronized Element getUpdatedSharedConfigurationData() {
        if (cachedElement == null) {
            Element  root = this.cfg.getConfigurationFragment("data", JbiCompProjectType.PROJECT_CONFIGURATION_NAMESPACE,true);    //NOI18N
            cachedElement = root;
        }
        return cachedElement;
    }
    
    private synchronized void fixSourceDirectories() {
        try     {
            FileObject prjDirFO = this.project.getProjectDirectory();
            FileObject confDirFO = FileUtil.createFolder(prjDirFO, JbiCompProjectProperties.CONF_DIR_VALUE);
            FileObject srcDirFO = FileUtil.createFolder(prjDirFO, JbiCompProjectProperties.SRC_DIR_VALUE);
            FileObject testDirFO = FileUtil.createFolder(prjDirFO, JbiCompProjectProperties.TEST_DIR_VALUE);
            // copy jbi.xml form old src/META-INF/jbi.xml to src/conf/META-INF/jbi.xml
            FileObject oldJbiXmlFO = prjDirFO.getFileObject("src/META-INF/jbi.xml");
            FileObject jbiXmlFO = confDirFO.getFileObject("META-INF/jbi.xml");
            if ( jbiXmlFO == null ) {
                FileObject jbiXmlMetaInfDir = FileUtil.createFolder(confDirFO, "META-INF");
                jbiXmlFO = FileUtil.copyFile(oldJbiXmlFO, jbiXmlMetaInfDir, "jbi");
            }
        } catch (IOException ex) {
            Exceptions.printStackTrace(ex);
        }
    }
    
    private synchronized EditableProperties getUpdatedProjectProperties() {
        EditableProperties cachedProperties = this.helper.getProperties(AntProjectHelper.PROJECT_PROPERTIES_PATH);
        if (cachedProperties.get(JbiCompProjectProperties.JAVADOC_ADDITIONALPARAM)==null) {
            cachedProperties.put(JbiCompProjectProperties.JAVADOC_ADDITIONALPARAM,"");    //NOI18N
        }
        if (cachedProperties.get("build.generated.dir")==null) { //NOI18N
            cachedProperties.put("build.generated.dir","${build.dir}/generated"); //NOI18N
        }
        if (cachedProperties.get("meta.inf.dir")==null) { //NOI18N
            cachedProperties.put("meta.inf.dir","${src.dir}/META-INF"); //NOI18N
        }
        
        if ( cachedProperties.getProperty(JbiCompProjectProperties.CONF_DIR) == null ) {
            // preview1 project properties. upgrade them to preview 2
            fixSourceDirectories();
            JbiCompProjectProperties.fixJbiProjectProperties(cachedProperties);
            JbiCompProjectProperties.loadFromJbiXml(this, cachedProperties);
            System.out.println("#### Fixed old project properties...");
            
        }
        
        return cachedProperties;
    }
    
    private static void copyDocument(Document doc, Element from, Element to) {
        NodeList nl = from.getChildNodes();
        int length = nl.getLength();
        for (int i=0; i< length; i++) {
            Node node = nl.item(i);
            Node newNode = null;
            switch (node.getNodeType()) {
            case Node.ELEMENT_NODE:
                Element oldElement = (Element) node;
                newNode = doc.createElementNS(JbiCompProjectType.PROJECT_CONFIGURATION_NAMESPACE,oldElement.getTagName());
                NamedNodeMap m = oldElement.getAttributes();
                Element newElement = (Element) newNode;
                for (int index = 0; index < m.getLength(); index++) {
                    Node attr = m.item(index);
                    newElement.setAttribute(attr.getNodeName(), attr.getNodeValue());
                }
                copyDocument(doc,oldElement,newElement);
                break;
            case Node.TEXT_NODE:
                Text oldText = (Text) node;
                newNode = doc.createTextNode(oldText.getData());
                break;
            case Node.COMMENT_NODE:
                Comment oldComment = (Comment) node;
                newNode = doc.createComment(oldComment.getData());
                break;
            }
            if (newNode != null) {
                to.appendChild(newNode);
            }
        }
    }
    
    private static Element updateMinAntVersion(final Element root, final Document doc) {
        NodeList list = root.getElementsByTagNameNS(JbiCompProjectType.PROJECT_CONFIGURATION_NAMESPACE,MINIMUM_ANT_VERSION_ELEMENT);
        if (list.getLength() == 1) {
            Element me = (Element) list.item(0);
            list = me.getChildNodes();
            if (list.getLength() == 1) {
                me.replaceChild(doc.createTextNode(JbiCompProjectGenerator.MINIMUM_ANT_VERSION), list.item(0));
                return root;
            }
        }
        assert false : "Invalid project file"; //NOI18N
        return root;
    }
    
    /**
     * Creates an default Notifier. The default notifier displays a dialog warning user about project update.
     * @return notifier
     */
    public static Notifier createDefaultNotifier() {
        return new Notifier() {
            public boolean canUpdate() {
                JButton updateOption = new JButton(NbBundle.getMessage(UpdateHelper.class, "CTL_UpdateOption"));
                updateOption.getAccessibleContext().setAccessibleDescription(NbBundle.getMessage(UpdateHelper.class, "AD_UpdateOption"));
                return DialogDisplayer.getDefault().notify(
                    new NotifyDescriptor(NbBundle.getMessage(UpdateHelper.class,"TXT_ProjectUpdate", BUILD_NUMBER),
                    NbBundle.getMessage(UpdateHelper.class,"TXT_ProjectUpdateTitle"),
                    NotifyDescriptor.DEFAULT_OPTION,
                    NotifyDescriptor.WARNING_MESSAGE,
                    new Object[] {
                    updateOption,
                    NotifyDescriptor.CANCEL_OPTION
                },
                    updateOption)) == updateOption;
            }
        };
    }
    
    /**
     * Interface used by the UpdateHelper to ask user about
     * the project update.
     */
    public static interface Notifier {
        /**
         * Asks user to update the project
         * @return true if the project should be updated
         */
        public boolean canUpdate();
    }
}
