/*
 * @(#)AbstractContextAwareAction.java        $Revision: 1.1 $ $Date: 2008/10/25 22:02:57 $
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

import java.awt.event.ActionEvent;
import java.util.logging.Logger;

import javax.swing.AbstractAction;

import org.openide.util.ContextAwareAction;
import org.openide.util.Lookup;
import org.openide.util.LookupEvent;
import org.openide.util.LookupListener;

import org.openesb.components.rules4jbi.netbeans.project.directory.DirectoryManager;

/**
 * Common superclass for all actions that are placed in the context menu
 * of the project's root node.
 * 
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1 $ $Date: 2008/10/25 22:02:57 $
 * 
 * @since 0.3
 */
public abstract class AbstractContextAwareAction extends AbstractAction implements ContextAwareAction, LookupListener {
    
    private static final Logger logger = Logger.getLogger(AbstractContextAwareAction.class.getName());
    
    private final Lookup context;
    
    private final Lookup.Result<DirectoryManager> result;

    public AbstractContextAwareAction(String name, Lookup context) {
        super(name);
        
        this.context = context;
        
        result = context.lookupResult(DirectoryManager.class);
        result.addLookupListener(this);
        resultChanged(null);
        
        logger.fine("Created instance of " + this.getClass().getName());
    }
    
    public final void actionPerformed(ActionEvent event) {
        logger.fine(this.getClass().getName() + " invoked");
        
        if (result.allInstances().size() != 1) {
            throw new AssertionError("There must be exactly one instance of DirectoryManager in the lookup"
                    + " for this action to be enabled");
        }
        
        DirectoryManager directoryManager = result.allInstances().iterator().next();
        
        performAction(directoryManager);
    }
    
    protected abstract void performAction(DirectoryManager directoryManager);

    /*
     * Once we assign an action of this type to the context menu of a Node, this method will be called
     * to create the actual instance representing the action, with the Node's lookup as a parameter.
     */
    public abstract AbstractContextAwareAction createContextAwareInstance(Lookup context);
    
    public final void resultChanged(LookupEvent event) {
        setEnabled(result.allInstances().size() == 1);
    }
}
