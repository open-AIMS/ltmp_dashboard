source("ui_body_dashboard.R")

tag_styles <- tags$style(HTML(
  '
    .skin-blue .main-header .logo {
      background-color: #00486A;
      color: #fff;
      border-bottom: 0;
      text-align: left;
      padding: 0px;
    }

    .skin-blue .main-header .logo:hover {
      background-color: #00486A;
      color: #fff;
    }

    .label-left .form-group {
      display: flex;              /* Use flexbox for positioning children */
      flex-direction: row;        /* Place children on a row (default) */
      width: 100%;                /* Set width for container */
      max-width: 400px;
    }

    .label-left .control-label {
      margin-right: 2rem;         /* Add spacing between label and slider */
      align-self: center;         /* Vertical align in center of row */
      text-align: right;
      flex-basis: 100px;          /* Target width for label */
    }

    .label-left .form-control {
      flex-basis: 300px;          /* Target width for slider */
    }

    .fa-circle-check {
      color: green; 
    }
        
    .fa-clock {
      color: orange;
    }

    .fa-circle-exclamation {
      color: orange;
    }

    .fa-circle-xmark {
      color: red;
    }

    #log_output {
      height: 200px;
      overflow-y: auto;
      display:flex;
      flex-direction: column-reverse;
    }
    #model_log_output {
      height: 200px;
      overflow-y: auto;
      display:flex;
      flex-direction: column-reverse;
    }

    .callout {
  border-left: 3px solid #BBB;
  background-color: #EEE;
  display: block;
  position: relative;
  overflow: auto;
  }

  .call-info {
  border: 1px solid #0288D1;
  background-color: #D3EFFF;
  font-size: 17px;
  border-left: 3px solid #0288D1;
}

.callout h4 {
  color: black;
font-weight: 500;
}
.call-info h4::before {
  content: "🛈" ;
  color: #0288D1;
  padding-right: 10px;
  vertical-align: middle;
  display: inline-block;

}

details {
margin-top: -10px;
}
details[open] {
  border: 1px solid #0288D1;
  background-color: #D3EFFF;
  border-left: 3px solid #0288D1;
  padding-left:5px;

}

ul {
padding-bottom:10px;
}

  //.details-info {
  //border: 1px solid #0288D1;
  //background-color: #D3EFFF;
  //border-left: 3px solid #0288D1;
  //padding-left:5px;
//}

summary {
 display:list-item;
}

.table-minimal table {
  border: 2px solid #000000;
  width: 100%;
  text-align: left;
  border-collapse: collapse;
}
.table-minimal td, .table-minimal th {
  border: 1px solid #000000;
  padding: 5px 4px;
}
.table-minimal tbody td {
  font-size: 13px;
}
.table-minimal thead {
  background: #D0E4F5;
  border-bottom: 1px solid #000000;
}
.table-minimal thead th {
  font-size: 15px;
  font-weight: bold;
  color: #000000;
  text-align: left;
  border-left: 1px solid #D0E4F5;
}
.table-minimal thead th:first-child {
  border-left: none;
}

.table-minimal tfoot td {
  font-size: 14px;
}

// buttons
.bttn[disabled] {
 cursor: not-allowed;
 color:grey;
 border-color:grey;
 background-color: white;
}

.btn-default {
   background-color: #1d89ff;
   color: #fff;
   border-color: #1d89ff;
}
.btn-default:hover {
   background-color: #fff;
   color: #1d89ff;
   border-color: #1d89ff;
}

.btn-disabled {
   background-color: #888;
   color: #000;
   border-color: #888;
}

.btn-disabled:hover {
   background-color: #888;
   color: #000;
   border-color: #888;
}
.btn-success {
   background-color: #00a65a;
   color: #fff;
   border-color: #00a65a;
}

.btn-success .fa-circle-check {
   color: #fff;
}

.btn-success:hover {
   background-color: #00a65a;
   color: #fff;
   border-color: #00a65a;
}

.btn-danger {
   background-color: #ab0f0e;
   color: #fff;
   border-color: #ab0f0e;
}

.btn-danger .fa-circle-check {
   color: #fff;
}

.btn-danger:hover {
   background-color: #ab0f0e;
   color: #fff;
   border-color: #ab0f0e;
}
.btn-warning {
   background-color: #cf4d03;
   color: #fff;
   border-color: #cf4d03;
}

.btn-warning .fa-circle-check {
   color: #fff;
}

.btn-warning:hover {
   background-color: #cf4d03;
   color: #fff;
   border-color: #cf4d03;
}
    '
)) 

body <- dashboardBody(
        tag_styles,
        # callout_style,
        tabItems(
                ## landing_tab,
                dashboard_tab
                ## data_tab,
                ## eda_tab,
                ## analysis_tab,
                ## manual_tab
        )
)
