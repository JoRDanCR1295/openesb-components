/*
 * @(#)DirectoryNodeBuilder.java        $Revision: 1.4 $ $Date: 2008/11/11 06:48:23 $
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

package org.openesb.components.rules4jbi.netbeans.project.nodes;

import java.awt.Image;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import javax.swing.Action;

import org.openide.filesystems.FileChangeListener;
import org.openide.filesystems.FileObject;
import org.openide.loaders.DataFolder;
import org.openide.loaders.DataObject;
import org.openide.nodes.FilterNode;
import org.openide.nodes.Node;
import org.openide.nodes.Node.PropertySet;
import org.openide.util.Utilities;
import org.openide.util.lookup.Lookups;
import org.openide.util.lookup.ProxyLookup;

import org.netbeans.spi.project.ui.PrivilegedTemplates;

import org.openesb.components.rules4jbi.netbeans.project.actions.ActionsFactory;
import org.openesb.components.rules4jbi.netbeans.project.directory.DirectoryManager;
import org.openesb.components.rules4jbi.netbeans.project.fileimport.FileType;

/**
 * Builder for a node that is a wrapper around a folder in the filesystem.
 * Allows to easily specify the display name, folder badge, which filetypes to show,
 * whether to show subfolders, etc.
 * <p>
 * This class can is designed for extension, but can also be used directly.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.4 $ $Date: 2008/11/11 06:48:23 $
 * 
 * @since 0.3
 */
public class DirectoryNodeBuilder {
    
    private static final Logger logger = Logger.getLogger(DirectoryNodeBuilder.class.getName());
    
    private final FileObject directory;
    
    private final Node rootNode;
    
    private final List<String> extensions;
    
    private final List<String> mimeTypes;

    private boolean showSubfolders = false;
    
    private String displayName = null;
    
    private Image badge = null;
    
    private FileType fileType = null;
    
    private DirectoryManager directoryManager = null;
    
    private PrivilegedTemplates privilegedTemplates = null;
    
    public DirectoryNodeBuilder(FileObject directory) {
        if (directory == null) {
            throw new NullPointerException("Parameter directory must not be null");
        }

        if (!directory.isValid() || directory.isVirtual()) {
            throw new IllegalArgumentException("Parameter directory is not valid");
        }

        if (!directory.isFolder()) {
            throw new IllegalArgumentException("Parameter directory must be a folder");
        }
        
        this.directory = directory;
        
        extensions = new ArrayList<String>();
        mimeTypes = new ArrayList<String>();

        DataFolder dataFolder = DataFolder.findFolder(directory);

        rootNode = dataFolder.getNodeDelegate();
    }
    
    public Node createNode() {
        return new DirectoryNode(rootNode, new DirectoryNodeChildren(rootNode));
    }

    public final DirectoryNodeBuilder displayName(String displayName) {
        this.displayName = displayName;
        
        return this;
    }

    public final DirectoryNodeBuilder badge(Image badge) {
        this.badge = badge;
        
        return this;
    }

    public final DirectoryNodeBuilder supportImport(FileType fileType, DirectoryManager directoryManager) {
        this.fileType = fileType;
        this.directoryManager = directoryManager;
        
        return this;
    }
    
    public final DirectoryNodeBuilder addFileChangeListener(FileChangeListener listener) {
        directory.addFileChangeListener(listener);
        
        return this;
    }
    
    public final DirectoryNodeBuilder privilegedTemplates(PrivilegedTemplates privilegedTemplates) {
        this.privilegedTemplates = privilegedTemplates;
        
        return this;
    }
    
    public final DirectoryNodeBuilder disablePrivilegedTemplates() {
        this.privilegedTemplates = new PrivilegedTemplates() {

            public String[] getPrivilegedTemplates() {
                return new String[0];
            }
        };
        
        return this;
    }
    
    public final DirectoryNodeBuilder showSubfolders() {
        showSubfolders = true;
        
        return this;
    }
    
    public final DirectoryNodeBuilder addExtension(String extension) {
        extensions.add(extension);
        
        return this;
    }
    
    public final DirectoryNodeBuilder addMIMEType(String mimeType) {
        mimeTypes.add(mimeType);
        
        return this;
    }
    
    
    class DirectoryNode extends FilterNode {

        DirectoryNode(Node node, DirectoryNodeChildren children) {
            super(node, children,
                    (privilegedTemplates != null)
                            ? (fileType == null 
                                    ? new ProxyLookup(node.getLookup(), Lookups.singleton(privilegedTemplates))
                                    : new ProxyLookup(node.getLookup(), Lookups.singleton(privilegedTemplates),
                                            Lookups.singleton(fileType), Lookups.singleton(directoryManager)))
                                    
                            : (fileType == null 
                                    ? node.getLookup()
                                    : new ProxyLookup(Lookups.singleton(fileType),
                                            Lookups.singleton(directoryManager),
                                            node.getLookup())));
        }

        @Override
        public Image getIcon(int type) {
            return badge != null
                    ? Utilities.mergeImages(super.getIcon(type), badge, 7, 7)
                    : super.getIcon(type);
        }

        @Override
        public Image getOpenedIcon(int type) {
            return badge != null
                    ? Utilities.mergeImages(super.getOpenedIcon(type), badge, 7, 7)
                    : super.getOpenedIcon(type);
        }
        
        @Override
        public String getDisplayName() {
            return displayName != null ? displayName : super.getDisplayName();
        }

        @Override
        public boolean canCopy() {
            return false;
        }

        @Override
        public boolean canCut() {
            return false;
        }

        @Override
        public boolean canDestroy() {
            return false;
        }

        @Override
        public boolean canRename() {
            return false;
        }
 
        @Override
        public Action[] getActions(boolean context) {
            if (context) {
                return super.getActions(true);

            } else if (fileType == null) {
                return super.getActions(false);
                
            } else {
                Action[] oldActions = super.getActions(false);
                Action[] actions = new Action[oldActions.length + 2];

                actions[0] = ActionsFactory.importFileAction();
                actions[1] = null;

                System.arraycopy(oldActions, 0, actions, 2, oldActions.length);

                return actions;
            }
        }
        
        @Override
        public PropertySet[] getPropertySets() {
            /*
             * We need to remove the PropertySet called "properties",
             * because we don't want to allow the modifications of the folder name
             * TODO: we should change it for a property, that does not allow modifications
             * and displays other useful information and combine it with the sorting property set
             * TODO: we need to support filtering by extension/mime-type
             */

            PropertySet[] oldPropertySets = super.getPropertySets();

            if (oldPropertySets == null || oldPropertySets.length == 0) {
                return oldPropertySets;
            }

            logger.fine("Old property sets size: " + oldPropertySets.length);
            List<PropertySet> newPropertySets = new ArrayList<PropertySet>();

            final String propertySetToRemove = "properties";

            for (PropertySet propertySet : oldPropertySets) {
                logger.fine("Analyzing property set: " + propertySet.getName());

                if (!propertySetToRemove.equals(propertySet.getName())) {
                    logger.fine("Retaining property set " + propertySet.getName());

                    newPropertySets.add(propertySet);
                }
            }

            return newPropertySets.toArray(new PropertySet[newPropertySets.size()]);
        }
    }
    
    
    class DirectoryNodeChildren extends FilterNode.Children {

        DirectoryNodeChildren(Node original) {
            super(original);
        }

        @Override
        protected Node[] createNodes(Node key) {
            DataObject dataObject = key.getLookup().lookup(DataObject.class);

            if (dataObject == null) {
                return new Node[0];
            }

            if (dataObject instanceof DataFolder) {
                if (showSubfolders) {
                    return new Node[] {
                            new FilterNode(key, new DirectoryNodeChildren(key), privilegedTemplates != null
                                    ? new ProxyLookup(Lookups.singleton(privilegedTemplates), key.getLookup())
                                    : key.getLookup())
                            };

                } else {
                    return new Node[0];
                }
            }

            if (mimeTypes.isEmpty() && extensions.isEmpty()) {
                return super.createNodes(key);
            }

            FileObject fileObject = dataObject.getPrimaryFile();

            if (mimeTypes.contains(fileObject.getMIMEType()) || extensions.contains(fileObject.getExt())) {
                return super.createNodes(key);

            } else {
                return new Node[0];
            }
        }
    }
}
