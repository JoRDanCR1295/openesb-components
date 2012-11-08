/*
 * @(#)NodeRenderer.java        $Revision: 1.1 $ $Date: 2008/11/12 08:26:24 $
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

package org.openesb.components.rules4jbi.netbeans.project.customizer;

import java.awt.Component;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.ListCellRenderer;

import org.openide.nodes.Node;
import org.openide.util.Utilities;

/**
 * Custom <code>ListCellRenderer</code> for displaying a list of <code>Node</code>s.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1 $ $Date: 2008/11/12 08:26:24 $
 * 
 * @see org.openide.nodes.Node
 * @since 0.3
 */
public class NodeRenderer extends JLabel implements ListCellRenderer {
    
    private static final long serialVersionUID = -2449366228226639442L;
    
    private static final Icon JAR_ICON = new ImageIcon(
            Utilities.loadImage("org/openesb/components/rules4jbi/netbeans/resources/jar.gif"));
    
    public NodeRenderer() {
        setOpaque(true);
    }
    
    public Component getListCellRendererComponent(JList list, Object value, int index,
            boolean isSelected, boolean cellHasFocus)
    {
        if (value instanceof Node) {
            final Node node = (Node) value;

            setIcon(JAR_ICON);
            setText(node.getDisplayName());
            
        } else {
            setIcon(null);
            setText(value == null ? " " : value.toString());
        }
        
        if (isSelected) {
            setBackground(list.getSelectionBackground());
            setForeground(list.getSelectionForeground());
            
        } else {
            setBackground(list.getBackground());
            setForeground(list.getForeground());
        }

        return this;
    }
}
