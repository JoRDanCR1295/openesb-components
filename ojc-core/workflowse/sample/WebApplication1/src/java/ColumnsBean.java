

import javax.faces.event.ActionEvent;
import javax.faces.model.ArrayDataModel;
import javax.faces.model.DataModel;

public class ColumnsBean
{
    private String[] index = {"1.", "2.", "3.", "4."};
    private String[] heading = {"Odd", "Even"};
    private DataModel rowModel = new ArrayDataModel(index);
    private DataModel columnsModel = new ArrayDataModel(heading);
    
    public DataModel getRowModel() {
        return rowModel;
    }
    
    public void setRowModel(DataModel rowModel) {
        this.rowModel = rowModel;
    }
    
    public DataModel getColumnsModel() {
        return columnsModel;
    }
    
    public void setColumnsModel(DataModel columnsModel) {
        this.columnsModel = columnsModel;
    }
}
