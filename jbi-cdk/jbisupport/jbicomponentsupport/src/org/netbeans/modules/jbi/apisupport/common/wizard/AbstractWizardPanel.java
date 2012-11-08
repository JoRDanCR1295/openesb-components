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
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import javax.swing.JComponent;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import org.openide.WizardDescriptor;
import org.openide.WizardValidationException;
import org.openide.util.HelpCtx;

/**
 * Wizard Panel base class.
 * @author chikkala
 */
public abstract class AbstractWizardPanel
        implements WizardDescriptor.Panel,
        WizardDescriptor.ValidatingPanel, WizardDescriptor.FinishablePanel {
    private AbstractWizardModel mWizModel;
    private AbstractWizardVisualPanel mVisualPanel;
    private final Set/*<ChangeListener>*/ mListeners = new HashSet(1);
    /** Create the wizard panel descriptor. */
    public AbstractWizardPanel() {
    }
    
    public void setWizardModel(AbstractWizardModel wizModel) {
        this.mWizModel = wizModel;
    }
    
    public AbstractWizardModel getWizardModel() {
        return this.mWizModel;
    }
    
    private AbstractWizardModel findWizardModel( Object settings ) {
        AbstractWizardModel wizModel = getWizardModel();
        if ( wizModel == null ) {
            wizModel = (AbstractWizardModel)
            ((WizardDescriptor)settings).getProperty(AbstractWizardModel.WIZARD_MODEL);
            setWizardModel(wizModel);
        }
        return wizModel;
    }
    
    protected abstract AbstractWizardVisualPanel createVisualPanel();
    
    protected AbstractWizardVisualPanel getVisualPanel() {
        if ( this.mVisualPanel == null ) {
            this.mVisualPanel = createVisualPanel();
        }
        return this.mVisualPanel;
    }
    
    public Component getComponent() {
        return getVisualPanel();
    }
    
    public HelpCtx getHelp() {
        return HelpCtx.DEFAULT_HELP;
    }
    
    public boolean isValid() {
        return getVisualPanel().isValidVisualPanel(this.mWizModel);
    }
    
    
    public final void addChangeListener(ChangeListener l) {
        synchronized (this.mListeners) {
            this.mListeners.add(l);
        }
    }
    public final void removeChangeListener(ChangeListener l) {
        synchronized (this.mListeners) {
            this.mListeners.remove(l);
        }
    }
    public final void fireChangeEvent() {
        Iterator it;
        synchronized (this.mListeners) {
            it = new HashSet(this.mListeners).iterator();
        }
        ChangeEvent ev = new ChangeEvent(this);
        while (it.hasNext()) {
            ((ChangeListener)it.next()).stateChanged(ev);
        }
    }
    
    public void readSettings(Object settings) {
        AbstractWizardModel wizModel = findWizardModel(settings);
        getVisualPanel().loadFromWizardModel(wizModel);
        
        // XXX hack, TemplateWizard in final setTemplateImpl() forces new wizard's title
        // this name is used in NewProjectWizard to modify the title
        Object substitute = ((JComponent)getVisualPanel()).getClientProperty("NewProjectWizard_Title"); // NOI18N
        if (substitute != null) {
            WizardDescriptor wizDescriptor = (WizardDescriptor)settings;
            wizDescriptor.putProperty("NewProjectWizard_Title", substitute); // NOI18N
        }
    }
    
    public void storeSettings(Object settings) {
        AbstractWizardModel wizModel = findWizardModel(settings);
        getVisualPanel().saveToWizardModel(wizModel);
        WizardDescriptor d = (WizardDescriptor)settings;
        ((WizardDescriptor)d).putProperty("NewProjectWizard_Title", null); // NOI18N
    }
    
    public boolean isFinishPanel() {
        return false;
    }
    
    public void validate() throws WizardValidationException {
        getVisualPanel().validateVisualPanel(this.mWizModel);
    }
    
}
