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

import java.io.File;
import org.openide.WizardDescriptor;

/**
 * Project wizard model
 * @author  chikkala
 */
public abstract class ProjectWizardModel extends AbstractWizardModel {
    public static final String PROJECT_WIZARD_TYPE = "PROJECT_WIZARD_TYPE";
    
    private File mProjectDir;
    private String mProjectName;
    private boolean mIsMainProject = false;
    
    /** Creates a new instance of NewAUWizardModel */
    public ProjectWizardModel(WizardDescriptor wiz) {
        this(wiz, PROJECT_WIZARD_TYPE );
    }
    /** Creates a new instance of NewAUWizardModel */
    public ProjectWizardModel(WizardDescriptor wiz, String wizType) {
        super(wiz,wizType);
    }
    
    /**
     * Getter for property projectDir.
     * @return Value of property projectDir.
     */
    public File getProjectDirectory() {
        return this.mProjectDir;
    }
    
    /**
     * Setter for property projectDir.
     * @param projectDir New value of property projectDir.
     */
    public void setProjectDirectory(File projectDir) {
        this.mProjectDir = projectDir;
    }
    
    /**
     * Getter for property projectName.
     * @return Value of property projectName.
     */
    public String getProjectName() {
        return this.mProjectName;
    }
    
    /**
     * Setter for property projectName.
     * @param projectName New value of property projectName.
     */
    public void setProjectName(String projectName) {
        this.mProjectName = projectName;
    }
    
    /**
     * Getter for property mainProject.
     * @return Value of property mainProject.
     */
    public boolean isMainProject() {
        return this.mIsMainProject;
    }
    
    /**
     * Setter for property mainProject.
     * @param mainProject New value of property mainProject.
     */
    public void setMainProject(boolean mainProject) {
        this.mIsMainProject = mainProject;
    }
    
}
