/*
 * @(#)TypeEditor.java        $Revision: 1.2 $ $Date: 2008/11/04 18:48:13 $
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

package org.openesb.components.rules4jbi.netbeans.wsdl.wizard.table;

import java.util.Arrays;
import org.openesb.components.rules4jbi.netbeans.util.chooser.FileChooserDialogDescriptor;
import org.openesb.components.rules4jbi.netbeans.util.chooser.FileChooserPanel;
import java.awt.Color;
import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.util.EventObject;
import java.util.logging.Logger;
import javax.swing.AbstractCellEditor;
import javax.swing.JButton;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.SwingConstants;
import javax.swing.border.LineBorder;
import javax.swing.table.TableCellEditor;
import org.netbeans.api.java.project.JavaProjectConstants;
import org.netbeans.api.project.Project;
import org.netbeans.api.project.ProjectUtils;
import org.netbeans.api.project.SourceGroup;
import org.netbeans.api.project.Sources;
import org.netbeans.spi.java.project.support.ui.PackageView;
import org.openesb.components.rules4jbi.netbeans.project.nodes.LibrariesNodeBuilder;
import org.openide.DialogDescriptor;
import org.openide.DialogDisplayer;
import org.openide.filesystems.FileObject;
import org.openide.nodes.Node;
import org.openide.nodes.NodeNotFoundException;
import org.openide.nodes.NodeOp;
import org.openide.util.NbBundle;

import static org.openesb.components.rules4jbi.shared.GlobalConstants.LIBRARIES_DIR;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.2 $ $Date: 2008/11/04 18:48:13 $
 * 
 * @since 0.1
 */
public class TypeEditor extends AbstractCellEditor implements TableCellEditor, ActionListener {
    
    private static final long serialVersionUID = -2449366228226639442L;
    
    private static final Logger logger = Logger.getLogger(TypeEditor.class.getName());
    
    private String value;

    private JButton button;
    
    private final FileChooserDialogDescriptor dialogDescriptor;
    
    private final Node rootNode;
    
    private final Node sourcePackages;
                
    private final Node librariesNode;
    
    public TypeEditor(final Project project) {
        button = new JButton();
        button.setBorder(new LineBorder(Color.BLACK));
        button.setBorderPainted(true);
        button.setContentAreaFilled(false);
        button.setHorizontalAlignment(SwingConstants.LEADING);
        button.setHorizontalTextPosition(SwingConstants.LEADING);

        /* Set font to the same one as it's renderer's font */
        button.setFont(new JTextField().getFont());

        button.setDefaultCapable(false);

        button.addActionListener(this);

        value = "";
        button.setText(value);
        
        /* Never returns null */
        Sources sources = ProjectUtils.getSources(project);

        SourceGroup[] javaSourceGroups = sources.getSourceGroups(JavaProjectConstants.SOURCES_TYPE_JAVA);

        if (javaSourceGroups == null || javaSourceGroups.length != 1) {
            throw new IllegalStateException("Project's sources were not correctly initialized");
        }

        sourcePackages = PackageView.createPackageView(javaSourceGroups[0]);
        
        librariesNode = new LibrariesNodeBuilder(project).createNode();
        
        rootNode = new TypeNode(sourcePackages, librariesNode);
        
        String title = NbBundle.getMessage(TypeEditor.class, "TypeEditor.javaFileChooserDialog.title");
        
        dialogDescriptor = new FileChooserDialogDescriptor(
                new FileChooserPanel(rootNode, title), title, "java", "class");
        
        dialogDescriptor.hideRootNode();
    }

    public Component getTableCellEditorComponent(JTable table, Object value, boolean isSelected,
            int row, int column)
    {
        this.value = (String) value;
        
        button.setText(this.value);
        
        return button;
    }
    
    public Object getCellEditorValue() {
        return value;
    }

    @Override
    public boolean isCellEditable(EventObject event) {
        if (event instanceof MouseEvent) {
            return ((MouseEvent) event).getClickCount() >= 2;
        }
        
        return true;
    }
    
    public void actionPerformed(ActionEvent event) {
        logger.finer("Current value is '" + value + "'");
        button.setText(value);

        Node currentNode = null;
        if (!"".equals(value)) {
            try {
                
                /*
                 * We will first look into the source packages node, as any class under this node
                 * takes precedence in the classloader's search path.
                 */
                
                String[] nodeNames = new String[] {getPackageName(value), getClassName(value)};

                currentNode = NodeOp.findPath(sourcePackages, nodeNames);

                logger.finer("Found current node in source packages: " + currentNode.getDisplayName());

            } catch (NodeNotFoundException e) {
                logger.finer("Could not find the current node in source packages; searching libraries...");
                
                String[] nodeNames = value.split("\\.");
                
                Node[] libraries = librariesNode.getChildren().getNodes(true);
                logger.finer("Searching following libraries: " + Arrays.toString(libraries));
                
                for (Node library : libraries) {
                    try {
                        currentNode = NodeOp.findPath(library, nodeNames);
                        
                        logger.finer("Found current node '" + currentNode.getDisplayName()
                                + "' in library '" + library.getDisplayName() + "'");
                        
                        break;
                        
                    } catch (NodeNotFoundException ex) {
                        logger.finer("Could not find the current node in library " + library.getDisplayName());
                    }
                }
            }
        }
        
        dialogDescriptor.clearSelection();
        
        if (currentNode != null) {
            dialogDescriptor.selectNode(currentNode);
        }
        
        Object result = DialogDisplayer.getDefault().notify(dialogDescriptor);

        if (DialogDescriptor.OK_OPTION.equals(result)) {
            logger.fine("File selection confirmed");

            FileObject selectedFile = dialogDescriptor.getSelectedFile();
            
            if (selectedFile == null) {
                throw new AssertionError("Selected file cannot be null");
            }
            
            if (!("java".equals(selectedFile.getExt()) || "class".equals(selectedFile.getExt()))) {
                throw new AssertionError("Only java and class files are allowed to be selected by this dialog");
            }

            logger.fine("Selected file: " + selectedFile.getNameExt());
            
            String[] nodeNames = NodeOp.createPath(dialogDescriptor.getSelectedNode(), rootNode);
            
            if (nodeNames.length == 0) {
                throw new AssertionError("The path to root node must contain at least the selected node itself");
            }
            
            final String type = nodeNames[0];
            int rootPackageNodeIndex = -1;
            
            if (LIBRARIES_DIR.equals(type)) {
                logger.finer("The selected node is a sub-node of libraries node");
                
                rootPackageNodeIndex = 2;
                
            } else {
                logger.finer("The selected node is a sub-node of source packages");
                
                rootPackageNodeIndex = 1;
            }
            
            String fullyQualifiedClassName = nodeNames[nodeNames.length - 1];
            
            for (int i = nodeNames.length - 2; i >= rootPackageNodeIndex; i--) {
                
                if (!nodeNames[i].equals("")) {
                    fullyQualifiedClassName = nodeNames[i] + "." + fullyQualifiedClassName;    

                } else {
                    /* we are dealing with the default package here */
                    
                    break;
                }
            }

            logger.fine("Fully qualified class name of the selected file is " + fullyQualifiedClassName);
            
            value = fullyQualifiedClassName;

            button.setText(value);
        }

        fireEditingStopped();
    }
    
    static String getPackageName(String fullyQualifiedClassName) {
        return isClassInDefaultPackage(fullyQualifiedClassName) 
                ? ""
                : extractPackageName(fullyQualifiedClassName);
    }
    
    static String getClassName(String fullyQualifiedClassName) {
        return isClassInDefaultPackage(fullyQualifiedClassName) 
                ? fullyQualifiedClassName 
                : extractClassName(fullyQualifiedClassName);
    }
    
    static boolean isClassInDefaultPackage(String fullyQualifiedClassName) {
        return fullyQualifiedClassName.indexOf(".") == -1;
    }

    static String extractPackageName(String fullyQualifiedClassName) {
        return fullyQualifiedClassName.substring(0, fullyQualifiedClassName.lastIndexOf("."));
    }

    static String extractClassName(String fullyQualifiedClassName) {
        return fullyQualifiedClassName.substring(fullyQualifiedClassName.lastIndexOf(".") + 1);
    }
}
