import config
with open("../booger.txt", "r") as file:
    content = file.read()
config.Mappings.parse(content)
