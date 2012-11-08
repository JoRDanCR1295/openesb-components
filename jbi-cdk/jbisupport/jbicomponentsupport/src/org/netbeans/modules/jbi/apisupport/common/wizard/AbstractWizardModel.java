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

package org.netbeans.modules.jbi.apisupport.common.wizard;

import com.sun.corba.se.impl.logging.UtilSystemException;
import java.io.IOException;
import java.util.HashSet;
import java.util.Set;
import org.netbeans.spi.project.ui.templates.support.Templates;
import org.openide.WizardDescriptor;
import org.openide.filesystems.FileObject;
import org.openide.loaders.DataFolder;
import org.openide.loaders.DataObject;


/**
 * TODO: create a Abstract class
 * @author  chikkala
 */
public abstract class AbstractWizardModel {
    public static final String WIZARD_MODEL = "WIZARD_MODEL";
    public static final String WIZARD_TYPE = "WIZARD_TYPE";
    public static final String GENERIC_WIZARD_TYPE = "GENERIC_WIZARD_TYPE";
    
    private transient WizardDescriptor mWiz;
    
    private Set mOpenFoSet;
    
    private Set mCreatedFoSet;
    
    
    /** Creates a new instance of NewAUWizardModel */
    public AbstractWizardModel(WizardDescriptor wiz) {
        this(wiz, GENERIC_WIZARD_TYPE );
    }
    /** Creates a new instance of NewAUWizardModel */
    public AbstractWizardModel(WizardDescriptor wiz, String wizType) {
        this.mWiz = wiz;
        this.mWiz.putProperty(WIZARD_TYPE,wizType);
        this.mWiz.putProperty(WIZARD_MODEL,this);
        initOpenFileObjectSet();
        initCreatedFileObjectSet();
    }
    
    public WizardDescriptor getWizardDescriptor() {
        return this.mWiz;
    }
    
    public String getWizardType() {
        if ( this.mWiz != null ) {
            return (String) this.mWiz.getProperty(WIZARD_TYPE);
        } else {
            return null;
        }
    }
    
    protected void initOpenFileObjectSet() {
        this.mOpenFoSet = new HashSet();
    }
    
    public Set getOpenFileObjectSet() {
        return this.mOpenFoSet;
    }
    
    public void addFileObjectForOpen(FileObject fo) {
        if ( fo != null ) {
            this.mOpenFoSet.add(fo);
        }
    }
    
    protected void initCreatedFileObjectSet() {
        this.mCreatedFoSet = new HashSet();
    }
    
    public Set getCreatedFileObjectSet() {
        return this.mCreatedFoSet;
    }
    
    public void addCreatedFileObject(FileObject fo) {
        if ( fo != null ) {
            this.mCreatedFoSet.add(fo);
        }
    }
    
    public FileObject getTemplate() {
        return Templates.getTemplate(this.mWiz);
    }
    
    public DataObject getTemplateDataObject() {
        FileObject fo = Templates.getTemplate(this.mWiz);
        if ( fo != null ) {
            // System.out.println("Template FO : " + fo.getPath());
        }
        DataObject dataObj = null;
        try {
            dataObj = DataObject.find(fo);
        } catch (Exception ex) {
            // System.out.println(ex);
            // ex.printStackTrace();
        }
        return dataObj;
    }
    
    public void setTargetFolder(FileObject targetFolderFO) {
        Templates.setTargetFolder(this.mWiz, targetFolderFO);
    }
    
    public FileObject getTargetFolder() {
        return Templates.getTargetFolder(this.mWiz);
    }
    
    public DataFolder getTargetDataFolder() {
        FileObject fo = Templates.getTargetFolder(this.mWiz);
        if ( fo != null ) {
            System.out.println("Target  Folder : " + fo.getPath());
        }
        
        DataFolder dataFolder = null;
        try {
            dataFolder = DataFolder.findFolder(fo);
        } catch (Exception ex) {
            System.out.println(ex);
            ex.printStackTrace();
        }
        return dataFolder;
    }
    
    public void setTargetName(String targetName) {
        Templates.setTargetName(this.mWiz, targetName);
    }
    
    public String getTargetName() {
        return Templates.getTargetName(this.mWiz);
    }
    
    public void initialize() {
        initOpenFileObjectSet();
        initCreatedFileObjectSet();
        initializeOthers();
    }
    
    public Set instantiate() throws IOException {
        instantiateOthers();
        return getCreatedFileObjectSet();
    }
    
    public void uninitialize() {
        uninitializeOthers();
    }
    
    public abstract void initializeOthers();
    
    public abstract void instantiateOthers() throws IOException;
    
    public abstract void uninitializeOthers();
    
    public void openArtifacts() {
        /*
        for ( Iterator itr = this.getOpenFileObjectSet().iterator(); itr.hasNext(); )
        {
            FileObject fo = (FileObject)itr.next();
            Utils.openFileObject(fo);
        }
         */
    }
    
}
