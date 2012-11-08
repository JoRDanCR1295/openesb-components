/*
 * @(#)Rules4JBICustomizerProvider.java        $Revision: 1.3 $ $Date: 2008/11/12 08:26:23 $
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

import java.awt.Dialog;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;
import javax.swing.JComponent;
import javax.swing.JPanel;
import org.netbeans.api.project.Project;
import org.netbeans.api.project.ProjectUtils;
import org.netbeans.spi.project.ui.CustomizerProvider;
import org.netbeans.spi.project.ui.support.ProjectCustomizer;
import org.openide.awt.StatusDisplayer;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.3 $ $Date: 2008/11/12 08:26:23 $
 * 
 * @since 0.1
 */
public class Rules4JBICustomizerProvider implements CustomizerProvider {
    
    private static final String GENERAL_CATEGORY_NAME = "general";
    
    private static final String RULES_ENGINE_CATEGORY_NAME = "engine";
    
    private static final String GENERAL_CATEGORY_DISPLAY_NAME = "General";
    
    private static final String RULES_ENGINE_CATEGORY_DISPLAY_NAME = "Rules Engine";
    
    private static final Logger logger = Logger.getLogger(Rules4JBICustomizerProvider.class.getName());
    
    private ProjectCustomizer.Category[] categories;
     
    private ProjectCustomizer.CategoryComponentProvider panelProvider;
    
    private CustomizerController controller;
    
    private final Project project;

    public Rules4JBICustomizerProvider(Project project) {
        this.project = project;
    }

    public void showCustomizer() {
        logger.fine("Showing customizer");

        init();
        
        Dialog dialog = ProjectCustomizer.createCustomizerDialog(
                categories, panelProvider, GENERAL_CATEGORY_NAME, new ActionListener() {
                    public void actionPerformed(ActionEvent e) {
                        controller.handleOkButtonPressed();
                    }
                }, null);
                
        dialog.addWindowListener(new WindowAdapter() {

            @Override
            public void windowClosing(WindowEvent e) {
                controller.handleWindowClosing();
            }

            @Override
            public void windowClosed(WindowEvent e) {
                controller.handleWindowClosed();
            }
        });
        
        dialog.setTitle("Project Properties - " + ProjectUtils.getInformation(project).getDisplayName());
        dialog.setModalityType(Dialog.ModalityType.APPLICATION_MODAL);
        
        StatusDisplayer.getDefault().setStatusText("Showing project properties");
        dialog.setVisible(true);
    }

    private void init() {
        controller = new CustomizerController(project);
        
        ProjectCustomizer.Category general = ProjectCustomizer.Category.create(
                GENERAL_CATEGORY_NAME, GENERAL_CATEGORY_DISPLAY_NAME, null);

        ProjectCustomizer.Category ruleEngine = ProjectCustomizer.Category.create(
                RULES_ENGINE_CATEGORY_NAME, RULES_ENGINE_CATEGORY_DISPLAY_NAME, null);

        categories = new ProjectCustomizer.Category[] {general, ruleEngine};

        Map<ProjectCustomizer.Category, JPanel> panels = new HashMap<ProjectCustomizer.Category, JPanel>();
        
        panels.put(general, controller.getGeneralPanel());
        panels.put(ruleEngine, controller.getRulesEnginePanel());

        panelProvider = new PanelProvider(panels);
    }
    
    private static class PanelProvider implements ProjectCustomizer.CategoryComponentProvider {

        private final JPanel EMPTY_PANEL = new JPanel();        
        
        private final Map<ProjectCustomizer.Category, JPanel> panels;

        private PanelProvider(Map<ProjectCustomizer.Category, JPanel> panels) {
            this.panels = panels;
        }

        public JComponent create(ProjectCustomizer.Category category) {
            JPanel panel = panels.get(category);
            
            return panel != null ? panel : EMPTY_PANEL;
        }
    }    
}
