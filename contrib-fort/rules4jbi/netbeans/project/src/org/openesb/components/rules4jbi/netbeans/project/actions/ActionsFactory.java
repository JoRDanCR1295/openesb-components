/*
 * @(#)ActionsFactory.java        $Revision: 1.1 $ $Date: 2008/10/25 22:02:57 $
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

import javax.swing.Action;

import org.openide.util.Utilities;

/**
 * Factory for actions that are accessible from the project's root node.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1 $ $Date: 2008/10/25 22:02:57 $
 * 
 * @since 0.3
 */
public final class ActionsFactory {

    private ActionsFactory() {}

    public static Action importFileAction() {
        return new ImportFileContextAwareAction(Utilities.actionsGlobalContext());
    }

    public static Action compileSchemasAction() {
        return new CompileSchemasContextAwareAction(Utilities.actionsGlobalContext());
    }
    
    public static Action createWSDLAction() {
        return new CreateWSDLContextAwareAction(Utilities.actionsGlobalContext());
    }
}
