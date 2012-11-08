/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.netbeans.modules.jbi.apisupport.config;

import java.io.File;
import java.io.InputStream;
import java.util.jar.Attributes;
import java.util.jar.Manifest;
import org.netbeans.api.project.Project;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.filesystems.XMLFileSystem;
import org.openide.util.Exceptions;
import org.xml.sax.SAXException;

/**
 *
 * @author chikkala
 */
public class ProjectConfigModel {

    public static final String MANIFEST_MF = "manifest.mf"; // NOI18N
    public static final String OPENIDE_MODULE_LAYER = "OpenIDE-Module-Layer"; // NOI18N
    private ComponentType mCompType;
    private String mCompName;
    private String mCompDesc;
    XMLFileSystem mLayerFS = null;
    FileObject mFSRoot = null;

    public ProjectConfigModel() {

    }

    public void loadModel(Project prj) {
        this.setCompType(ComponentType.BINDING);
        this.setCompName("MyComponent");
        this.setCompDesc("Component description");

        try {
            File layerFile = getLayerFilePath(prj);
            this.mLayerFS = new XMLFileSystem(layerFile.toURI().toASCIIString());
            this.mFSRoot = mLayerFS.getRoot();
        } catch (Exception ex) {
            ex.printStackTrace();
            return;
        }
        
        FileObject compFolder = getJBIComponentFolder();
        String type = (String)compFolder.getAttribute("type");
        //TODO add the valueOf to the Enum 
        if ( "Engine".equalsIgnoreCase(type)) {
            this.mCompType = ComponentType.ENGINE;            
        } else if ( "Binding".equalsIgnoreCase(type) ) {
            this.mCompType = ComponentType.BINDING;
        } else {
            System.out.println("JBI LAYER: Unknown component type");
        }
        this.mCompName = (String)compFolder.getAttribute("id");
        this.mCompDesc = (String)compFolder.getAttribute("description");
    }
    
    private FileObject getJBIComponentFolder() {
        FileObject compFolder = null;
        FileObject jbiCompsFolder = this.mFSRoot.getFileObject("JbiComponents");
        if ( jbiCompsFolder != null ) {
            FileObject[] children = jbiCompsFolder.getChildren();
            for ( FileObject child : children ) {
                if ( child.isFolder()) {
                    String type = (String)child.getAttribute("type");
                    if ( "Engine".equalsIgnoreCase(type) || "Binding".equalsIgnoreCase(type)) {
                        compFolder = child;
                        break;
                    }
                }
            }
        }
        return compFolder;
    }

    public void saveModel() {

    }

    public String getCompDesc() {
        return mCompDesc;
    }

    public void setCompDesc(String compDesc) {
        this.mCompDesc = compDesc;
    }

    public String getCompName() {
        return mCompName;
    }

    public void setCompName(String compName) {
        this.mCompName = compName;
    }

    public ComponentType getCompType() {
        return mCompType;
    }

    public void setCompType(ComponentType compType) {
        this.mCompType = compType;
    }

    public File getLayerFilePath(Project prj) {

        File layerXmlFile = null;
        FileObject mfFO = prj.getProjectDirectory().getFileObject(MANIFEST_MF);
        if (mfFO == null) {
            return null;
        }
        Manifest mf = null;
        InputStream mfIS = null;
        FileObject layerFO = null;

        try {
            mfIS = mfFO.getInputStream();
            mf = new Manifest(mfIS);
            Attributes attr = mf.getMainAttributes();
            String layerPath = attr.getValue(OPENIDE_MODULE_LAYER);
            layerFO = prj.getProjectDirectory().getFileObject(layerPath);
        } catch (Exception ex) {
            ex.printStackTrace();
        } finally {
            if (mfIS != null) {
                try {
                    mfIS.close();
                } catch (Exception ex) {
                // ignore
                }
            }
        }

        if (layerFO != null) {
            layerXmlFile = FileUtil.toFile(layerFO);
        }
        return layerXmlFile;
    }

    public static enum ComponentType {

        ENGINE, BINDING
    }
}
