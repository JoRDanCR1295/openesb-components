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

import java.awt.Component;
import java.io.IOException;
import java.text.MessageFormat;
import java.util.NoSuchElementException;
import java.util.Set;
import javax.swing.JComponent;
import javax.swing.event.ChangeListener;
import org.openide.WizardDescriptor;

/**
 * Wizard to create a new Jbi Component project.
 * @author chikkala
 */
public abstract class AbstractWizardIterator
        implements WizardDescriptor.InstantiatingIterator {
    private static final long serialVersionUID = 1L;
    
    private transient int mIndex;
    private transient WizardDescriptor.Panel[] mPanels;
    private transient WizardDescriptor mWizDesc;
    private transient AbstractWizardModel mWizModel;
    
    protected abstract WizardDescriptor.Panel[] createPanels();
    protected abstract String[] createSteps();
    protected abstract AbstractWizardModel createWizardModel(WizardDescriptor wizDesc);
    
    protected AbstractWizardModel getWizardModel() {
        return this.mWizModel;
    }
    
    protected WizardDescriptor getWizardDescriptor() {
        return this.mWizDesc;
    }
    
    
    // You can keep a reference to the TemplateWizard which can
    // provide various kinds of useful information such as
    // the currently selected target name.
    // Also the panels will receive wiz as their "settings" object.
    public void initialize(WizardDescriptor wiz) {
        this.mWizDesc = wiz;
        this.mWizModel = createWizardModel(this.mWizDesc);
        this.mWizModel.initialize();
        this.mIndex = 0;
        this.mPanels = createPanels();
        // Make sure list of steps is accurate.
        String[] steps = createSteps();
        for (int i = 0; i < this.mPanels.length; i++) {
            Component c = this.mPanels[i].getComponent();
            if (steps[i] == null) {
                // Default step name to component name of panel.
                // Mainly useful for getting the name of the target
                // chooser to appear in the list of steps.
                steps[i] = c.getName();
            }
            String compName = c.getName();
            if ( compName == null ) {
//                Utils.debug("Wizard Visual Panel Title is null for " + steps[i]);
                c.setName(steps[i]);
            }
            
            if (c instanceof JComponent) { // assume Swing components
                JComponent jc = (JComponent)c;
                // Step #.
                jc.putClientProperty("WizardPanel_contentSelectedIndex", new Integer(i)); // NOI18N
                // Step name (actually the whole list for reference).
                jc.putClientProperty("WizardPanel_contentData", steps); // NOI18N
            }
        }
    }
    
    public Set/*<FileObject>*/ instantiate() throws IOException {
        Set resultSet = this.mWizModel.instantiate();
        return resultSet;
    }
    
    public void uninitialize(WizardDescriptor wiz) {
        this.mWizModel.uninitialize();
        this.mWizDesc = null;
        this.mPanels = null;
        this.mWizModel = null;
    }
    
    public abstract String getNameFormat();
    
    public String name() {
        return MessageFormat.format(getNameFormat(), new Object[]
        {new Integer(this.mIndex + 1), new Integer(this.mPanels.length) });
    }
    
    public boolean hasNext() {
        return this.mIndex < this.mPanels.length - 1;
    }
    public boolean hasPrevious() {
        return this.mIndex > 0;
    }
    public void nextPanel() {
        if (!hasNext()) {
            throw new NoSuchElementException();
        }
        this.mIndex++;
    }
    public void previousPanel() {
        if (!hasPrevious()) {
            throw new NoSuchElementException();
        }
        this.mIndex--;
    }
    
    public WizardDescriptor.Panel current() {
        return this.mPanels[this.mIndex];
    }
    
    // If nothing unusual changes in the middle of the wizard, simply:
    public final void addChangeListener(ChangeListener l) {
    }
    public final void removeChangeListener(ChangeListener l) {
    }
    
}
