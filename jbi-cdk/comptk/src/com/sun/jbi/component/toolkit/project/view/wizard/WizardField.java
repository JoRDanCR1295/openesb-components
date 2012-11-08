/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)WizardField.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.project.view.wizard;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.Enumeration;
import java.util.EventListener;
import javax.swing.AbstractAction;
import javax.swing.AbstractButton;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.plaf.metal.MetalBorders;
import javax.swing.text.StyledDocument;
import com.sun.jbi.component.toolkit.project.CDK;
import com.sun.jbi.component.toolkit.project.GenerateTokens.Tkn;
import com.sun.jbi.component.toolkit.project.view.DescriptionField.Text;

/**
 * 
 * @author Kevan Simpson
 */
public class WizardField extends EntryPanel {
    private static final int COLS = 30;
    
    private JPanel mMainPanel;
    private JLabel mLabel;
    private JTextField mTextField;
    private JTextArea mTextArea;
    private JList mList;
    private DefaultListModel mListModel;
    private JRadioButton[] mRadios;
    private ButtonGroup mBtnGroup;
    private JButton mBrowseBtn;
    private String mName, mDescription;
    private Text mType;
    
    private Tkn mTkn;
    
    /**
     * 
     */
    public WizardField(CreateWizard wiz, Tkn tkn, String name, String desc) {
        this(wiz, tkn, name, desc, Text.field);
    }

    /**
     * 
     */
    public WizardField(CreateWizard wiz, Tkn tkn, String name, String desc, Text type) {
        super(wiz, tkn, name, desc, type);
    }

    /**
     * 
     */
    public WizardField(CreateWizard wiz, Tkn tkn, String name, String desc, 
                            Text type, String[] items) {
        super(wiz, tkn, name, desc, type, items);
    }
    
    public void addListener(EventListener el) {
        switch (mType) {
            case label:
            case file:
            case field: 
            case area:  {
                break;
            }
            case radio: {
                for (Enumeration<AbstractButton> btns = mBtnGroup.getElements();
                     btns.hasMoreElements();) {
                    AbstractButton ab = btns.nextElement();
                    ab.addActionListener((ActionListener) el);
                }
                break;
            }
        }

    }
    
    /** @see javax.swing.JComponent#setEnabled(boolean) */
    @Override
    public void setEnabled(boolean enabled) {
        switch (mType) {
            case file: {
                mBrowseBtn.setEnabled(enabled);
                break;
            }
        }
        super.setEnabled(enabled);
    }

    /** @see com.sun.jbi.component.toolkit.project.view.wizard.EntryPanel#captureInput() */
    @Override
    protected boolean captureInput() {
        System.out.println("Capturing token - "+ mTkn +" = "+ getValue());
        String value = getValue();
        if (mType == Text.file) {
            value = value.replace("\\", "\\\\");
        }
        wizard().getTokens().put(mTkn, value);
        return true;
    }

    @Override
    protected JPanel createEntryPanel() {
        return null;
    }
    /** @see com.sun.jbi.component.toolkit.project.view.wizard.EntryPanel#giveFocus() */
    @Override
    protected void giveFocus() {
        switch (mType) {
            case label:
            case field: {
                mTextField.requestFocusInWindow();
                break;
            }
            case area:  {
                mTextArea.requestFocusInWindow();
                break;
            }
            case file: {
                if (mBrowseBtn.isEnabled()) {
                    mBrowseBtn.requestFocusInWindow();
                }
                break;
            }
        }
    }


    @Override
    protected void updateInstructions(StyledDocument doc) {
    }

    public String getValue() {
        switch (mType) {
            case label:
            case file:
            case field: {
                return mTextField.getText();
            }
            case area:  {
                return mTextArea.getText();
            }
            case radio: {
                for (Enumeration<AbstractButton> btns = mBtnGroup.getElements();
                     btns.hasMoreElements();) {
                    AbstractButton ab = btns.nextElement();
                    if (ab.isSelected()) {
                        return ab.getText();
                    }
                }
                // drop through and return null if no radio selected
            }
            default: return null;
        }
    }
    
    public void setValue(String... text) {
        if (text != null) {
            String one = (text.length == 0) ? "" : text[0];
            switch (mType) {
                case label:
                case file:
                case field: {
                    mTextField.setText(one);
                    break;
                }
                case area:  {
                    mTextArea.setText(one);
                    break;
                }
                case list: {
                    String select = null;
                    mListModel.clear();
                    for (String str : text) {
                        mListModel.addElement(str);
                        if (select == null) {
                            select = str;
                        }
                    }
                    mList.setSelectedValue(select, true);
                    break;
                }
                case radio: {
                    for (Enumeration<AbstractButton> btns = mBtnGroup.getElements();
                         btns.hasMoreElements();) {
                        AbstractButton ab = btns.nextElement();
                        if (ab.getText().equals(one)) {
                            ab.setSelected(true);
                            break;
                        }
                    }
                    break;
                }
            }
        }
    }

    Text getType() {
        return mType;
    }

    /** @see com.sun.jbi.component.toolkit.project.view.BasePanel#init(java.lang.Object[]) */
    protected void init(Object... params) {
        setWizard((CreateWizard) params[0]);
        mTkn = (Tkn) params[1];
        mName = String.valueOf(params[2]);
        mDescription = String.valueOf(params[3]);
        mType = (Text) params[4];
        String[] items = null;
        if (mType == Text.radio) {
            items = (String[]) params[5];
        }
        setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
        setFocusable(false);
        mMainPanel = new JPanel(new BorderLayout());
        JPanel fldPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 10, 10));
        mLabel = new JLabel(mName, JLabel.RIGHT);
        mLabel.setFocusable(false);
        mLabel.setToolTipText("<html>"+ mDescription +"</html>");
        fldPanel.add(mLabel);
        
        mTextField = new JTextField(COLS);
        mTextField.setToolTipText(mDescription);
        switch (mType) {
            case field: {
                fldPanel.add(mTextField);
                break;
            }
            case area:  {
                mTextArea = new JTextArea(2, COLS);
                mTextArea.setToolTipText(mDescription);
                mTextArea.setLineWrap(true);
                mTextArea.setBorder(MetalBorders.getTextFieldBorder());
                fldPanel.add(mTextArea);
                break;
            }
            case label: {
                mTextField.setEditable(false);
                mTextField.setBackground(Color.white);
                fldPanel.add(mTextField);
                break;
            }
            case list: {
                mListModel = new DefaultListModel();
                mList = new JList(mListModel);
                mList.setLayoutOrientation(JList.VERTICAL_WRAP);
                mList.setFont(mTextField.getFont());
                mList.setFixedCellWidth(mTextField.getPreferredSize().width);
                mList.setBackground(mTextField.getBackground());
                mList.setToolTipText(mDescription);
                mList.setVisibleRowCount(4);
                mList.setBorder(MetalBorders.getTextFieldBorder());
//                mList.addKeyListener(this);
//                mList.addPropertyChangeListener(getApp());  // may not need
                fldPanel.add(mList);
                break;
            }
            case radio: {
                JPanel pnlRadio = new JPanel(new FlowLayout(FlowLayout.LEFT, 10, 0));
                pnlRadio.setBorder(MetalBorders.getTextFieldBorder());
//                pnlRadio.setOpaque(true);
//                pnlRadio.setBackground(Color.white);
//                pnlRadio.setBorder(MetalBorders.getTextFieldBorder());
                int len = items.length;
                mRadios = new JRadioButton[len];
                mBtnGroup = new ButtonGroup();
                for (int i = 0; i < len; i++) {
                    mRadios[i] = new JRadioButton(items[i]);
                    mRadios[i].setOpaque(false);
                    mBtnGroup.add(mRadios[i]);
                    pnlRadio.add(mRadios[i]);
                }
                fldPanel.add(pnlRadio);
                break;
            }
            case file: {
                JPanel pnlFile = new JPanel(new FlowLayout(FlowLayout.RIGHT, 10, 0));
//                pnlFile.setOpaque(true);
//                pnlFile.setBackground(Color.white);
//                pnlFile.setBorder(MetalBorders.getTextFieldBorder());
                mTextField.setEditable(false);
                mTextField.setBackground(Color.white);
                pnlFile.add(mTextField);
                mBrowseBtn = new JButton(new AbstractAction("Browse...") {
                    public void actionPerformed(ActionEvent e) {
                        JFileChooser jfc = new JFileChooser(
                                new File(System.getProperty(CDK.CDK_HOME)));
                        jfc.setDialogTitle("Select the component project root...");
                        jfc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
                        int opt = jfc.showOpenDialog(null);
                        if (opt == JFileChooser.APPROVE_OPTION) {
                            File file = jfc.getSelectedFile();
                            if (file != null) {
                                mTextField.setText(file.getAbsolutePath());
                                wizard().getTokens().put(mTkn, file.getAbsolutePath());
                            }
                        }
                    }
                });
                pnlFile.add(mBrowseBtn);
                fldPanel.add(pnlFile);
                break;
            }
        }

        fldPanel.setToolTipText(mDescription);

        mMainPanel.add(fldPanel, BorderLayout.CENTER);
        add(mMainPanel);
        
    }
}
