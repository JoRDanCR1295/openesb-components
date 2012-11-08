/*
 * @(#)AbstractWizardPanel.java        $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:19 $
 * 
 * Copyright (c) 2008 Milan Fort (http://www.milanfort.com/). All rights reserved.
 * 
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License (the "License"). You may not use this file except
 * in compliance with the License.
 * 
 * You can obtain a copy of the license at http://www.sun.com/cddl/cddl.html.
 * See the License for the specific language governing permissions and limitations
 * under the License.
 */

package org.openesb.components.rules4jbi.netbeans.util.wizard;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import javax.swing.JComponent;
import javax.swing.event.ChangeListener;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import org.openide.WizardDescriptor;
import org.openide.WizardValidationException;
import org.openide.util.ChangeSupport;
import org.openide.util.HelpCtx;

/**
 * Common superclass for all wizard panels.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:19 $
 * 
 * @since 0.1
 */
public abstract class AbstractWizardPanel implements WizardDescriptor.FinishablePanel<WizardDescriptor>,
        WizardDescriptor.ValidatingPanel<WizardDescriptor>, PropertyChangeListener, DocumentListener
{
    public abstract String getName();

    public abstract JComponent getComponent();
    
    private volatile boolean valid = false;
    
    private ChangeSupport changeSupport = new ChangeSupport(this);
    
    public boolean isFinishPanel() {
        return false;
    }

    public void validate() throws WizardValidationException {}

    public HelpCtx getHelp() {
        return HelpCtx.DEFAULT_HELP;
    }

    public final boolean isValid() {
        return valid;
    }

    public final void addChangeListener(ChangeListener listener) {
        changeSupport.addChangeListener(listener);
    }

    
    public final void removeChangeListener(ChangeListener listener) {
        changeSupport.removeChangeListener(listener);
    }

    /**
     * Subclasses must implement this method and return <code>true</code> if their associated visual panel
     * contains only valid data; <code>false</code> otherwise.
     * 
     * Implementation note: this method is called from within a document listener, so the implementations
     * should never try to modify the contents of a text component that fired the event; otherwise,
     * the program could deadlock. The implementations should only check validity and preferably also
     * display error message (using <code>showErrorMessage()</code>) if the panel is not valid.
     * 
     * @return <code>true</code> if the associated visual panel contains only valid data;
     * <code>false</code> otherwise.
     */ 
    protected abstract boolean checkValidity();
    
    private void stateChanged() {
        final boolean oldState = valid;
        
        valid = checkValidity();
        
        if (valid != oldState) {
            changeSupport.fireChange();
        }
    }
    
    /*
     * This method demonstrates how to support all event changes
     * for all new listener types added later
     */
    public final void propertyChange(PropertyChangeEvent event) {
        stateChanged();
    }

    public final void changedUpdate(DocumentEvent event) {
        /* Should never occur, plain text components that we use do not fire these events */
    }

    public void insertUpdate(DocumentEvent event) {
        stateChanged();
    }

    public void removeUpdate(DocumentEvent event) {
        stateChanged();
    }
    
    private WizardDescriptor settings = null;
    
    public void readSettings(WizardDescriptor settings) {
        this.settings = settings;
        
        /*
         * Forces to check the state (validity) at the beginning of the panel's creation,
         * when no listeners were notified yet.
         */ 
        stateChanged();
    }
    
    protected void showErrorMessage(String errorMessage) {
        if (settings != null) {
            settings.putProperty("WizardPanel_errorMessage", errorMessage);
        }
    }
}
