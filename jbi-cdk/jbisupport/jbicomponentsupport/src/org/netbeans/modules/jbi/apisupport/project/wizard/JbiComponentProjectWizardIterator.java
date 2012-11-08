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


package org.netbeans.modules.jbi.apisupport.project.wizard;

import java.awt.Component;
import java.io.IOException;
import java.util.Collections;
import java.util.NoSuchElementException;
import java.util.Set;
import javax.swing.JComponent;
import javax.swing.event.ChangeListener;
import org.netbeans.modules.jbi.apisupport.common.wizard.AbstractWizardIterator;
import org.netbeans.modules.jbi.apisupport.common.wizard.AbstractWizardModel;
import org.netbeans.modules.jbi.apisupport.common.wizard.AbstractWizardPanel;
import org.openide.WizardDescriptor;
import org.openide.WizardDescriptor.Panel;
import org.openide.cookies.OpenCookie;
import org.openide.loaders.*;
import org.openide.util.NbBundle;
import org.openide.filesystems.*;
import org.openide.WizardDescriptor;
import org.netbeans.spi.project.ui.templates.support.Templates;


/** A template wizard iterator (sequence of panels).
 * Used to fill in the panels in the New wizard.
 * Associate this to a template inside a layer using the
 * Sequence of Panels extra property.
 * Create one or more panels from template as needed too.
 *
 * @author chikkala
 */
public class JbiComponentProjectWizardIterator extends AbstractWizardIterator {
    
    private static final long serialVersionUID = 1L;
    
    private String mCompType;
    
    public JbiComponentProjectWizardIterator(String compType) {
        super();
        this.mCompType = compType;
    }
    
    protected WizardDescriptor.Panel[] createPanels() {
        return new Panel[]
        {
            new BasicJbiComponentProjectPanel()
        };
        
    }
    
    protected String[] createSteps() {
        return new String[]
        {
            NbBundle.getMessage(JbiComponentProjectWizardIterator.class,"BasicCompWizard.step.NameAndLoc")
//            NbBundle.getMessage(JbiComponentProjectWizardIterator.class,"LBL_ComponentDescriptionStep"),
        };
        
    }
    
    public String getNameFormat() {
        return NbBundle.getMessage(JbiComponentProjectWizardIterator.class, "TITLE_x_of_y");
    }
    
    protected AbstractWizardModel createWizardModel(WizardDescriptor wizDesc) {
        return JbiComponentProjectWizardModel
                .createComponentProjectWizardModel(wizDesc, this.mCompType);
    }
    
    public static JbiComponentProjectWizardIterator createServiceEngineProjectWizardIterator() {
        return new JbiComponentProjectWizardIterator(
                JbiComponentProjectWizardModel.SERVICE_ENGINE_COMP_TYPE);
    }
    
    public static JbiComponentProjectWizardIterator createBindingComponentProjectWizardIterator() {
        return new JbiComponentProjectWizardIterator(
                JbiComponentProjectWizardModel.BINDING_COMPONENT_COMP_TYPE);
    }
    
    
}
