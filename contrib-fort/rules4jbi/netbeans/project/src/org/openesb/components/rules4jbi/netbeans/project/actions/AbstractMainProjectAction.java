/*
 * @(#)AbstractMainProjectAction.java        $Revision: 1.1 $ $Date: 2008/10/25 22:02:58 $
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

package org.openesb.components.rules4jbi.netbeans.project.actions;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.logging.Logger;

import javax.swing.SwingUtilities;

import org.openide.util.HelpCtx;
import org.openide.util.actions.CallableSystemAction;

import org.netbeans.api.project.Project;
import org.netbeans.api.project.ui.OpenProjects;

import org.openesb.components.rules4jbi.netbeans.project.directory.DirectoryManager;

/**
 * Common superclass for all main project sensitive actions. These actions can be integrated
 * into the IDE through the system filesystem (layer.xml) to a menu and/or to a toolbar.
 * They will be enabled iff the current main project is a <code>Rules4JBIProject</code>.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1 $ $Date: 2008/10/25 22:02:58 $
 * 
 * @since 0.3
 */
public abstract class AbstractMainProjectAction extends CallableSystemAction implements PropertyChangeListener {

    private static final Logger logger = Logger.getLogger(AbstractMainProjectAction.class.getName());
    
    /* Forcing the JVM to load the class */
    private static Class<DirectoryManager> directoryManagerClass = DirectoryManager.class;
    
    private DirectoryManager directoryManager = null;
    
    public AbstractMainProjectAction() {
        updateStatus();
        
        OpenProjects.getDefault().addPropertyChangeListener(this);
    }
    
    private void updateStatus() {
        Project mainProject = OpenProjects.getDefault().getMainProject();
        
        if (mainProject == null) {
            setEnabledFromEventDispatchThread(false);
            return;
        }
        
        directoryManager = mainProject.getLookup().lookup(DirectoryManager.class);
        setEnabledFromEventDispatchThread(directoryManager != null);
    }

    private void setEnabledFromEventDispatchThread(final boolean enabled) {
        
        SwingUtilities.invokeLater(new Runnable() {

            public void run() {
                setEnabled(enabled);
            }
        });
    }
    
    public final void propertyChange(PropertyChangeEvent event) {
        logger.fine("Received property change event " + event.getPropertyName());
        
        if (OpenProjects.PROPERTY_MAIN_PROJECT.equals(event.getPropertyName())) {
            logger.finer("Updating status of " + this.getClass().getName());
            
            updateStatus();
        }
    }
    
    @Override
    public final void performAction() {
        performAction(directoryManager);
    }

    protected abstract void performAction(DirectoryManager directoryManager);
    
    @Override
    public HelpCtx getHelpCtx() {
        return HelpCtx.DEFAULT_HELP;
    }

    @Override
    protected boolean asynchronous() {
        return false;
    }
}
