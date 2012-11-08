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
 * @(#)TableBean.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package woodstock.table;

import java.io.Serializable;
import java.util.ArrayList;
import com.sun.data.provider.TableDataProvider;
import com.sun.data.provider.impl.ObjectArrayDataProvider;
import com.sun.data.provider.impl.ObjectListDataProvider;


public class TableBean implements Serializable {
    
    private TableDataProvider provider = null; // Data provider.
    
        protected static final Name[] names = {
            new Name("George", "Washington"),
            new Name("Ben", "Franklin"),
            new Name("John", "Kennedy"),
            new Name("Dwight", "Eisenhower"),
            new Name("Ronald", "Reagan"),
            new Name("Bill", "Clinton"),
            new Name("Samuel", "Adams")
    };
    
    // Default constructor.
    public TableBean() {
    }
    
        // Get data provider.
    public TableDataProvider getNames() {
        // Create List with all names.
        ArrayList newNames = new ArrayList();
        for (int i = 0; i<names.length ; i++) {
            newNames.add(names[i]);
        }
        provider = new ObjectListDataProvider(newNames);
        return provider;
    }
    
    public String add() {
        System.out.println("Add called...");
        return "";
    }

    public String edit() {
        System.out.println("Edit called...");
        return "";
    }    

    public String delete() {
        System.out.println("Delete called...");
        return "";
    }    
    
    
}
