/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2007 Sun Microsystems, Inc. All rights reserved.
 *
 * The contents of this file are subject to the terms of either the GNU
 * General Public License Version 2 only ("GPL") or the Common
 * Development and Distribution License("CDDL") (collectively, the
 * "License"). You may not use this file except in compliance with the
 * License. You can obtain a copy of the License at
 * http://www.netbeans.org/cddl-gplv2.html
 * or nbbuild/licenses/CDDL-GPL-2-CP. See the License for the
 * specific language governing permissions and limitations under the
 * License.  When distributing the software, include this License Header
 * Notice in each file and include the License file at
 * nbbuild/licenses/CDDL-GPL-2-CP.  Sun designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Sun in the GPL Version 2 section of the License file that
 * accompanied this code. If applicable, add the following below the
 * License Header, with the fields enclosed by brackets [] replaced by
 * your own identifying information:
 * "Portions Copyrighted [year] [name of copyright owner]"
 *
 * Contributor(s):
 *
 * The Original Software is NetBeans. The Initial Developer of the Original
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2006 Sun
 * Microsystems, Inc. All Rights Reserved.
 *
 * If you wish your version of this file to be governed by only the CDDL
 * or only the GPL Version 2, indicate your decision by adding
 * "[Contributor] elects to include this software in this distribution
 * under the [CDDL or GPL Version 2] license." If you do not indicate a
 * single choice of license, a recipient has the option to distribute
 * your version of this file under either the CDDL, the GPL Version 2 or
 * to extend the choice of license to its licensees as provided above.
 * However, if you add GPL Version 2 code and therefore, elected the GPL
 * Version 2 license, then the option applies only if the new code is
 * made subject to such option by the copyright holder.
 */

package org.netbeans.modules.jbi.apisupport.xmlmv;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/**
 * The class simplifies use of a combo box to show/set value of an item
 *
 * @author pfiala
 */
public abstract class ItemCheckBoxHelper implements ActionListener, Refreshable {
    private JCheckBox checkBox;
    private XmlMultiViewDataSynchronizer synchronizer;

    /**
     * Constructor initializes object by combo box and data object which will be handled
     *
     * @param synchronizer
     * @param checkBox   handled JComboBox.
     */
    public ItemCheckBoxHelper(XmlMultiViewDataSynchronizer synchronizer, JCheckBox checkBox) {
        this.synchronizer = synchronizer;
        this.checkBox = checkBox;
        checkBox.addActionListener(this);
        setValue(getItemValue());
    }

    /**
     * Invoked when an action occurs on a combo box.
     */
    public final void actionPerformed(ActionEvent e) {
        final boolean value = getValue();
        if (value != getItemValue()) {
            setItemValue(value);
            synchronizer.requestUpdateData();
        }
    }

    /**
     * Selects the item value in check box.
     *
     * @param itemValue value of item to be selected in check box
     */
    public void setValue(boolean itemValue) {
        checkBox.setSelected(itemValue);
    }

    /**
     * Check box getter
     *
     * @return handled check box
     */
    public JCheckBox getCheckBox() {
        return checkBox;
    }

    /**
     * Retrieves the text value selected in the check box.
     *
     * @return selected item of the check box
     */
    public boolean getValue() {
        return checkBox.isSelected();
    }

    /**
     * Called by the helper in order to retrieve the value of the item.
     *
     * @return value of the handled item.
     */
    public abstract boolean getItemValue();

    /**
     * Called by the helper in order to set the value of the item
     *
     * @param value new value of the hanlded item
     */
    public abstract void setItemValue(boolean value);

    public void refresh() {
        setValue(getItemValue());
    }
}
