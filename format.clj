
; This is what Enlive produces
{ :tag :container,
  :content (
    { :tag :ident-name,     :content ("system")}
    { :tag :leaf,
      :content
      ( {:tag :ident-name,  :content ("host-name")}
        {:tag :type-simple, :content ("string")}
        {:tag :description, :content ("Hostname for this system")})}
    { :tag :leaf,
      :content
      ( {:tag :ident-name,  :content ("alias-name")}
        {:tag :type-simple, :content ("string")}
        {:tag :description, :content ("Alias name for this system")})})}

; A more "associative" format
{ :tag :container,
  :content (
    { :tag :ident-name, :content ("system")}
    { :tag :leaf,
      :content ( 
        { :ident-name "host-name"
          :type-simple :string
          :description "Hostname for this system" }

        { :ident-name   "alias-name"
          :type-simple  :string
          :description  "Alias name for this system"}
      ) 
    }
  ) 
}

; v3
{ :container [
    { :ident-name "system"
      :leaf [
        { :ident-name   "host-name"
          :type-simple  :string
          :description  "Hostname for this system" }

        { :ident-name   "alias-name"
          :type-simple  :string
          :description  "Alias name for this system" }
      ]
      :leaf-list [
        { :ident-name   "host-name"
          :type-simple  :string
          :description  "Hostname for this system" }

        { :ident-name   "props"
          :type-simple  :string
          :description  "List of properties for the system" }
      ]
      :list [
        { :ident-name   "interface"
          :key          :name
          :description  "Hostname for this system" 
          :leaf [
            { :ident-name "name" :type-simple  :string }
            { :ident-name "type" :type-simple  :string }
            { :ident-name "mtu"  :type-simple  :int32  }
          ]
        }
      ]
    }
  ]
}
