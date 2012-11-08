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

package org.netbeans.modules.jbi.apisupport.project.ui.customizer;

import java.awt.Dialog;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.text.MessageFormat;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.ResourceBundle;
import javax.swing.JComponent;
import javax.swing.JPanel;
import org.netbeans.api.project.Project;
import org.netbeans.api.project.ProjectUtils;
import org.netbeans.modules.jbi.apisupport.project.JbiCompProject;
import org.netbeans.modules.jbi.apisupport.project.UpdateHelper;
import org.netbeans.spi.project.support.ant.GeneratedFilesHelper;
import org.netbeans.spi.project.support.ant.PropertyEvaluator;
import org.netbeans.spi.project.support.ant.ReferenceHelper;
import org.netbeans.spi.project.ui.CustomizerProvider;
import org.netbeans.spi.project.ui.support.ProjectCustomizer;
import org.openide.util.HelpCtx;
import org.openide.util.NbBundle;


/** Customization of JbiComponent project
 *
 * @author chikkala, j2seproject
 */
public class CustomizerProviderImpl implements CustomizerProvider {
    
    private final Project project;
    private final UpdateHelper updateHelper;
    private final PropertyEvaluator evaluator;
    private final ReferenceHelper refHelper;
    private final GeneratedFilesHelper genFileHelper;
    
    private ProjectCustomizer.Category categories[];
    private ProjectCustomizer.CategoryComponentProvider panelProvider;
    
    // Option indexes
    private static final int OPTION_OK = 0;
    private static final int OPTION_CANCEL = OPTION_OK + 1;
    
    // Option command names
    private static final String COMMAND_OK = "OK";          // NOI18N
    private static final String COMMAND_CANCEL = "CANCEL";  // NOI18N
    
    private static Map /*<Project,Dialog>*/project2Dialog = new HashMap();
    
    public CustomizerProviderImpl(Project project, UpdateHelper updateHelper, PropertyEvaluator evaluator, ReferenceHelper refHelper, GeneratedFilesHelper genFileHelper) {
        this.project = project;
        this.updateHelper = updateHelper;
        this.evaluator = evaluator;
        this.refHelper = refHelper;
        this.genFileHelper = genFileHelper;
    }
    
    public void showCustomizer() {
        showCustomizer( null );
    }
    
    
    public void showCustomizer( String preselectedCategory ) {
        showCustomizer( preselectedCategory, null );
    }
    
    public void showCustomizer( String preselectedCategory, String preselectedSubCategory ) {
        
        Dialog dialog = (Dialog)project2Dialog.get(project);
        if ( dialog != null ) {
            dialog.show();
            return;
        } else {
            CustomizerUIModel uiProperties = new CustomizerUIModel( (JbiCompProject)project, updateHelper, evaluator, refHelper, genFileHelper );
            init( uiProperties );
            
            OptionListener listener = new OptionListener( project, uiProperties );
            if (preselectedCategory != null && preselectedSubCategory != null) {
                for (int i=0; i<categories.length; i++ ) {
                    if (preselectedCategory.equals(categories[i].getName())) {
                        JComponent component = panelProvider.create(categories[i]);
                        if (component instanceof SubCategoryProvider) {
                            ((SubCategoryProvider)component).showSubCategory(preselectedSubCategory);
                        }
                        break;
                    }
                }
            }
            dialog = ProjectCustomizer.createCustomizerDialog( categories, panelProvider, preselectedCategory, listener, null );
            dialog.addWindowListener( listener );
            dialog.setTitle( MessageFormat.format(
                NbBundle.getMessage( CustomizerProviderImpl.class, "LBL_Customizer_Title" ), // NOI18N
                new Object[] { ProjectUtils.getInformation(project).getDisplayName() } ) );
            
            project2Dialog.put(project, dialog);
            dialog.show();
        }
    }
    
    // Names of categories
    private static final String BUILD_CATEGORY = "BuildCategory";
    private static final String RUN_CATEGORY = "RunCategory";
    
    private static final String GENERAL = "General";
    private static final String SOURCES = "Sources";
    private static final String LIBRARIES = "Libraries";
    
    private static final String BUILD = "Build";
    private static final String BUILD_TESTS = "BuildTests";
    private static final String JAR = "Jar";
    private static final String JAVADOC = "Javadoc";
    private static final String RUN = "Run";
    private static final String RUN_TESTS = "RunTests";
    
    private static final String SLIBS_CAT = "SharedLibs";
    
    private static final String COMPONENT_CATEGORY = "ComponentCategory";
    private static final String COMPONENT_DESC = "ComponentDescription";
    
    private void init( CustomizerUIModel uiProperties ) {
        
        ResourceBundle bundle = NbBundle.getBundle( CustomizerProviderImpl.class );
        
        /*
        ProjectCustomizer.Category general = ProjectCustomizer.Category.create(
                BUILD,
                bundle.getString( "LBL_Config_Build" ),
                null,
                null );
         */
        
        
        ProjectCustomizer.Category sources = ProjectCustomizer.Category.create(
            SOURCES,
            bundle.getString("LBL_Config_Sources"),
            null,
            null);
        
        ProjectCustomizer.Category libraries = ProjectCustomizer.Category.create(
            LIBRARIES,
            bundle.getString( "LBL_Config_Libraries" ), // NOI18N
            null,
            null );
        
        ProjectCustomizer.Category compDescription = ProjectCustomizer.Category.create(
            COMPONENT_DESC,
            bundle.getString( "Customizer.category.component.description.LBL" ), // NOI18N
            null,
            null );
        
        ProjectCustomizer.Category build = ProjectCustomizer.Category.create(
            BUILD,
            bundle.getString( "LBL_Config_Build" ), // NOI18N
            null,
            null);
        ProjectCustomizer.Category jar = ProjectCustomizer.Category.create(
            JAR,
            bundle.getString( "LBL_Config_Jar" ), // NOI18N
            null,
            null );
        ProjectCustomizer.Category javadoc = ProjectCustomizer.Category.create(
            JAVADOC,
            bundle.getString( "LBL_Config_Javadoc" ), // NOI18N
            null,
            null );
        
        ProjectCustomizer.Category run = ProjectCustomizer.Category.create(
            RUN,
            bundle.getString( "LBL_Config_Run" ), // NOI18N
            null,
            null );
        ProjectCustomizer.Category runTests = ProjectCustomizer.Category.create(
            RUN_TESTS,
            bundle.getString( "Customizer.category.component.test.LBL" ), // NOI18N
            null,
            null);
        
        ProjectCustomizer.Category buildCategory = ProjectCustomizer.Category.create(
            BUILD_CATEGORY,
            bundle.getString( "LBL_Config_BuildCategory" ), // NOI18N
            null,
            new ProjectCustomizer.Category[] { build, jar, javadoc }  );
        
        ProjectCustomizer.Category slibsCat = ProjectCustomizer.Category.create(
            SLIBS_CAT,
            bundle.getString( "Customizer.category.slibs.LBL" ), // NOI18N
            null,
            null);
        
        categories = new ProjectCustomizer.Category[] {
            sources,
            libraries,
            slibsCat,
            compDescription,
            buildCategory,
            run,
            runTests
        };
        
        Map<ProjectCustomizer.Category, JComponent> panels = new HashMap<ProjectCustomizer.Category, JComponent>();
        panels.put( sources, new CustomizerSources( uiProperties ) );
        panels.put( libraries, new CustomizerLibraries( uiProperties ) );
        
        panels.put( slibsCat, new CustomizerSharedLibs(uiProperties ) );
        
        panels.put( compDescription, new CustomizerJbiDescriptor( uiProperties ) );
        panels.put( build, new CustomizerCompile( uiProperties ) );
        panels.put( jar, new CustomizerJar( uiProperties ) );
        panels.put( javadoc, new CustomizerJavadoc( uiProperties ) );
        panels.put( run, new CustomizerRun( uiProperties ) );
        panels.put( runTests, new CustomizerTest(uiProperties ) );
        
        panelProvider = new PanelProvider( panels );
        
    }
    
    private static class PanelProvider implements ProjectCustomizer.CategoryComponentProvider {
        
        private JPanel EMPTY_PANEL = new JPanel();
        
        private Map /*<Category,JPanel>*/ panels;
        
        PanelProvider( Map panels ) {
            this.panels = panels;
        }
        
        public JComponent create( ProjectCustomizer.Category category ) {
            JComponent panel = (JComponent)panels.get( category );
            return panel == null ? EMPTY_PANEL : panel;
        }
        
    }
    
    /** Listens to the actions on the Customizer's option buttons */
    private class OptionListener extends WindowAdapter implements ActionListener {
        
        private Project project;
        private CustomizerUIModel uiProperties;
        
        OptionListener( Project project, CustomizerUIModel uiProperties ) {
            this.project = project;
            this.uiProperties = uiProperties;
        }
        
        // Listening to OK button ----------------------------------------------
        
        public void actionPerformed( ActionEvent e ) {
            // Store the properties into project
            uiProperties.save();
            
            // Close & dispose the the dialog
            Dialog dialog = (Dialog)project2Dialog.get( project );
            if ( dialog != null ) {
                dialog.hide();
                dialog.dispose();
            }
        }
        
        // Listening to window events ------------------------------------------
        
        public void windowClosed( WindowEvent e) {
            project2Dialog.remove( project );
        }
        
        public void windowClosing(WindowEvent e) {
            //Dispose the dialog otherwsie the {@link WindowAdapter#windowClosed}
            //may not be called
            Dialog dialog = (Dialog)project2Dialog.get( project );
            if ( dialog != null ) {
                dialog.hide();
                dialog.dispose();
            }
        }
    }
    
    static interface SubCategoryProvider {
        public void showSubCategory(String name);
    }
    
    
}
