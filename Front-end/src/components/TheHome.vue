<template>
  <main>
    <div class="container">
      <div class="center div-error">
        {{ erros }}
      </div>

      <div class="div-form">
        <form @submit.prevent="salvar">
          <label>Nome</label>
          <input type="text" placeholder="Nome" v-model="contato.nome">

          <label>Tipo</label>
          <select v-model="contato.id_tipo" class="browser-default">
            <option value="0" disabled selected>Selecione uma opção</option>
            <option v-for="tipo in tipos" :key="tipo.id" v-bind:value="tipo.id">
              {{ tipo.descricao }}
            </option>
          </select>

          <label>Contato</label>
          <input type="text" placeholder="Contato" v-model="contato.contato">

          <div class="center">
            <button class="btn-small btn">Salvar<i class="material-icons left">save</i></button>
          </div>
        </form>
      </div>
    </div>

    <div class="container">
      <hr>
      <table>
        <thead>
          <tr>
            <th>Nome</th>
            <th>Tipo</th>
            <th>Contato</th>
            <th></th>
          </tr>
        </thead>

        <tbody>
          <tr v-for="contato in contatos" :key="contato.id">
            <td>{{ contato.nome }}</td>
            <td>{{ contato.tipo }}</td>
            <td>{{ contato.contato }}</td>
            <td>
              <button @click="editar(contato)" class="btn-floating blue btn-line"><i
                  class="material-icons">edit</i></button>
              <button @click="remover(contato)" class="btn-floating red btn-line"><i
                  class="material-icons">delete_forever</i></button>
            </td>
          </tr>
        </tbody>
      </table>
    </div>
  </main>
</template>


<script>
import Contato from '../services/Contatos.js'

export default {

  data() {
    return {
      contato: {
        id: '',
        nome: '',
        id_tipo: 0,
        tipo: '',
        contato: '',
        select: ''
      },
      tipos: [],
      contatos: [],
      erros: ''
    }
  },

  mounted() {
    this.allTipos()
    this.listar()
  },

  methods: {
    limpar() {
      this.contato = {}
      this.erros = ''
      this.listar()
    },

    allTipos() {
      Contato.listarTipos().then(response => {
        if (response.status == 200) {
          if (response.data.success) {
            this.tipos = response.data.data
          } else {
            this.erros = response.data.message
          }
        } else {
          this.erros = `${response.status} -> ${response.statusText}`
        }
      }).catch(e => {
        this.erros = e.message
      })
    },

    listar() {
      Contato.listar().then(response => {
        if (response.status == 200) {
          if (response.data.success) {
            this.contatos = response.data.data
          } else {
            this.erros = response.data.message
          }
        } else {
          this.erros = `${response.status} -> ${response.statusText}`
        }
      }).catch(e => {
        this.erros = e.message
      })
    },

    salvar() {
      if (!this.contato.id) {
        Contato.salvar(this.contato).then(response => {
          if (response.status == 200) {
            if (response.data.success) {
              this.limpar()
              alert(response.data.message)
            } else {
              this.erros = response.data.message
            }
          } else {
            this.erros = `${response.status} -> ${response.statusText}`
          }
        }).catch(e => {
          this.erros = e.message
        })

      } else {
        Contato.atualizar(this.contato).then(response => {
          if (response.status == 200) {
            if (response.data.success) {
              alert(response.data.message)
              this.limpar()
            } else {
              this.erros = response.data.message
            }
          } else {
            this.erros = `${response.status} -> ${response.statusText}`
          }
        }).catch(e => {
          this.erros = e.message
        })
      }
    },

    editar(contato) {
      this.contato = contato
    },

    remover(contato) {
      if (confirm('Deseja excluir o contato?')) {
        Contato.apagar(contato).then(response => {
          if (response.status == 200) {
            if (response.data.success) {
              alert(response.data.message)
              this.limpar()
            } else {
              this.erros = response.data.message
            }
          } else {
            this.erros = `${response.status} -> ${response.statusText}`
          }
        }).catch(e => {
          this.erros = e.message
        })
      }
    }
  }
}
</script>

<style scoped>
.div-error {
  margin-top: 10px;
  background-color: rgb(233, 129, 129);
  color: white;
}

.div-form {
  margin-top: 20px;
}

select {
  background-color: var(--color-background);
  border-bottom: 1px solid grey;
  margin-bottom: 10px;
}

.btn {
  margin-top: 20px;
  margin-bottom: 30px;
  border-radius: 15px;
}

.btn-line {
  margin: 0px 5px;
}
</style>
