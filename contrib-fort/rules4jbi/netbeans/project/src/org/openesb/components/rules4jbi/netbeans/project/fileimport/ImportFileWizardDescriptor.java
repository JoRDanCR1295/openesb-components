/*
 * @(#)ImportFileWizardDescriptor.java        $Revision: 1.2 $ $Date: 2008/11/05 21:34:51 $
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

package org.openesb.components.rules4jbi.netbeans.project.fileimport;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import org.openide.WizardDescriptor;
import org.openide.util.NbBundle;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.2 $ $Date: 2008/11/05 21:34:51 $
 * 
 * @since 0.3
 */
public class ImportFileWizardDescriptor extends WizardDescriptor {
    
    private static final Logger logger = Logger.getLogger(ImportFileWizardDescriptor.class.getName());
    
    private final FileTypeWizardPanel panel1 = new FileTypeWizardPanel();
    
    private final NameAndLocationWizardPanel panel2 = new NameAndLocationWizardPanel();
    
    public ImportFileWizardDescriptor(final FileType preselectedFileType) {
        initProperties();
        
        List<Panel<WizardDescriptor>> panels = new ArrayList<Panel<WizardDescriptor>>();
        
        if (preselectedFileType == null) {
            panels.add(panel1);
        
        } else {
            putProperty("WizardPanel_contentSelectedIndex", 1);
            putProperty(Constants.PROPERTY_FILE_TYPE, preselectedFileType);
        }

        panels.add(panel2);
        
        /* At this point, the readSettings() method of the first wizard panel is called */
        setPanelsAndSettings(new ArrayIterator<WizardDescriptor>(panels), this);
        
        /* {0} will be replaced by WizardDesriptor.Panel.getComponent().getName() */
        setTitleFormat(new MessageFormat("{0}"));
        setTitle(NbBundle.getMessage(ImportFileWizardDescriptor.class, "ImportFileWizardDescriptor.title"));
    }
    
    private void initProperties() {
        logger.fine("Initializing properties of import file wizard");
        
        putProperty("WizardPanel_autoWizardStyle", Boolean.TRUE);
        putProperty("WizardPanel_contentDisplayed", Boolean.TRUE);
        putProperty("WizardPanel_contentNumbered", Boolean.TRUE);
        putProperty("WizardPanel_contentData", new String[] {panel1.getName(), panel2.getName()});
        
        logger.fine("Import File Wizard Properties: " + getProperties().toString());
    }
}
